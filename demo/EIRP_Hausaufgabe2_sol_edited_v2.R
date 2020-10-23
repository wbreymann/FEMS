################################################################################
# Beginn des Lösungsskripts
################################################################################

# Deaktiviere Warnungen temporär
options(warn=-1)
library("devtools")
rm(list=ls())
devtools::load_all()

# Lade nötige Pakete
# library(rActus)
# library(rflPortfolio)
# library(rflSimulation)
# Definiere Analysezeitpunkt
t0 <- "2018-01-02"	
# Sammle Energiepreise (verwende eurostat package)
library(eurostat)

# GAS: we use
#      - 28 Europa-Länder Durchschnittspreis
#      - Preis inklusive aller Steuern
#      - Preis für Konsum-Band:  1 000 GJ < Consumption < 10 000 GJ
gas.raw <- get_eurostat(id = "nrg_pc_203")
gas <- subset(as.data.frame(gas.raw),
         geo=="EU28" & time>"2007-12-31" & unit=="KWH" 
         & tax=="I_TAX" & currency=="EUR" & consom == 4142902)
gas.annual <- data.frame(
  time = sort(grep("-01-01", gas$time, fixed=TRUE, value=TRUE)),
  price = tapply(gas$values, substring(gas$time, 1, 4), mean))
gas.ts <- timeSeries(data=as.numeric(gas.annual[,2]), charvec=gas.annual[,1])
plot(gas.ts)
# EL: we use
#      - 28 Europa-Länder Durchschnittspreis
#      - Preis inklusive aller Steuern
#      - Preis für Konsum-Band: Band IB : 20 MWh < Consumption < 500 MWh
el.raw <- get_eurostat(id = "nrg_pc_205")
el <- subset(as.data.frame(el.raw),
         geo=="EU28" & time>"2007-12-31" & unit=="KWH" 
         & tax=="I_TAX" & currency=="EUR" & consom == 4162902)
el.annual <- data.frame(
  time = sort(grep("-01-01", el$time, fixed=TRUE, value=TRUE)),
  price = tapply(el$values, substring(el$time, 1, 4), mean))
el.ts <- timeSeries(data=as.numeric(el.annual[,2]), charvec=as.character(el.annual[,1]))
el.ts <- timeSeries(data=as.numeric(el.annual[,2]), el.annual[,1])
plot(el.ts)

# Passe Normalverteilung an Renditen an
# ret.gas <- diff(log(gas.annual$price)) # log. Renditen
ret.gas <- returns(gas.ts) # mit Befehl aus timeSeries Package
plot(ret.gas)
gas.norm <- c(mean(ret.gas), sd(ret.gas))
# ret.el <- diff(log(el.annual$price))
ret.el <- returns(el.ts)
plot(ret.el)
el.norm <- c(mean(ret.el), sd(ret.el))

# Schreibe Funktion zum Generieren von Preisreihen
p.sim <- function(n, P.0, t.0, dt, mu, sig) {
  return(timeSeries(
    data = P.0 * exp(cumsum(c(0, rnorm(n, mu, sig)))), # Geometrische Zufallsbewegung
    # data = P.0 * cumprod(1 + mu + sig*c(0, rnorm(n))), # mit relativen Renditen
    timeSequence(t.0, by=dt, length.out=n+1)
  ))
}
# Generiere Gaspreise
gas.p <- p.sim(10, tail(gas.annual, 1)$price, tail(gas.annual,1)$time,
            "1 years", gas.norm[1], gas.norm[2])
plot(gas.p)
# Generiere Index-Objekt aus Zeitreihe
gas.idx <- Index(
      MarketObjectCode = "GAS",
      Data=list(Dates=paste0(time(gas.p), "T00"), Values=series(gas.p)))

# Generiere Strompreise
el.p <- p.sim(10, tail(el.annual,1)$price, tail(el.annual,1)$time,
            "1 years", el.norm[1], el.norm[2])
plot(el.p)
el.idx <- Index(
      MarketObjectCode = "EL",
      Data = list(Dates=paste0(time(el.p), "T00"), Values=series(el.p)))

# Zinskurve
yc.tnr <- c("1M","10Y")
yc.rts <- c(0.02,0.02)
yc <- YieldCurve(MarketObjectCode = "MARKTZINS", 
               ReferenceDate = paste0(as.character(t0),"T00"), 
               Tenors = yc.tnr, Rates = yc.rts)
plot(yc)
# generiere Marktumgebung
rf <- RFConn(list(gas.idx, el.idx, yc))
rf

# Generiere Diskontierungsmethode
diskont <- DcEngine(dc.spread=0.0, RiskFactorObjectLink="MARKTZINS")
set(diskont, rf)

# Modellierung des Kraftwerks
# Was kostet die Anlage?
investition.nominal <- 100000000
# Definiere Kredit-Kontrakt
Kredit <- Lam(
     ContractID = "Kredit01",
     Currency = "EUR",
     ContractRole = "RPL", # Passiva (Liability)
     StatusDate       = "2018-01-01T00",
     ContractDealDate = "2018-01-02T00",
     InitialExchangeDate = "2018-01-03T00",
     MaturityDate = "2028-01-02T00",
     NotionalPrincipal = investition.nominal,
     NominalInterestRate = 0.02, # Marktzinsniveau per t0
     DayCountConvention = "30E/360",
     CycleOfInterestPayment = "1Y-",
     CycleAnchorDateOfInterestPayment = "2019-01-03T00",
     CycleOfPrincipalRedemption = "1Y-",
     CycleAnchorDateOfPrincipalRedemption = "2019-01-03T00"
)

# Operations Zeitachse
ops.times <- timeSequence(from=timeDate("2018-01-03"), by="1 years", length.out=11)

# Definiere Investitions-Pattern
inv.func <- function(model, params) {
  timeSeries(seq(investition.nominal, 0, length.out=11), ops.times)
}

# Definiere Investitions-Kontrakt
inv <- Operations(ContractID="Invest01", Currency="EUR", InvestPattern=inv.func)

# Definiere Gas-Einkaufs-Pattern
gas.func <- function(model,params) {
  timeSeries(-24*300*100*1000*(1/0.4) *
             valueAt(get(model, "GAS"), paste0(time(gas.p), "T00")),
             ops.times)
}
# Definiere Operations-Kontrakt mit CashFlowPattern
gas <- Operations(ContractID="Gas01", Currency="EUR", CashFlowPattern=gas.func)
# Definiere Gas-Einkaufs-Pattern
el.func <- function(model,params) {
  timeSeries(24*300*100*1000*
             valueAt(get(model, "EL"), paste0(time(el.p), "T00")),
             ops.times)
}
# Definiere Investitions-Kontrakt
el <- Operations(ContractID="El01", Currency="EUR", CashFlowPattern=el.func)
# Erstelle Portfolio
ptf <- Portfolio()
add(ptf, list(Kredit, gas, el, inv))
summary(ptf)
# Link zu Marktumgebung
set(ptf, rf)

# Erstelle Analysestruktur
Bilanz <- Tree(list(
branches = list( 
  Bilanz = c("Aktiva", "Passiva", "ProfitAndLoss"),
  Aktiva = "Investition",
  Passiva = "Kredit",
  ProfitAndLoss = "Produktion"
),
leafs = list(
  Investition = get(inv, "ContractID"),
  Kredit = get(Kredit, "ContractID"),
  Produktion = c(get(gas, "ContractID"), get(el, "ContractID"))
)
))
Bilanz

# Generiere Events
evL <- events(ptf, t0)
events <- as.data.frame(events(ptf, t0))
dim(events)
events
# Welche Event-Typen werden generiert?
table(events$Type)

# Erklärung
# AD0: Analysezeitpunkt (pro Kontrakt)
# DPR: Abschreibungen auf Investition
# IED: Initial Exchange: Auszahlung Kredit und Investition
# IP: Interest Payment: Zinszahlungen aus Kredit
# MD: Rückzahlung des verbleibenden Kapitals
# PR: Tilgung, Amortisation (Principal Redemption)
# OPS: Operationelles Erträge oder Aufwände

# Buchhalterische Effekte
# Event     | Liquidität | Einkommen | Bilanz
# AD0       |            |           |
# DPR       |            |    x      |   x
# IED       |     x      |           |   x
# IP        |     x      |    x      |
# PR        |     x      |           |   x
# MD        |     x      |           |   x
# OPS       |     x      |    x      |    

# Analyse von Bilanz
# Analyse-Zeitstrahl
by <- timeSequence(t0, by="1 years", length.out=6)
tb <- timeBuckets(by, bucketLabs=2018:2022)  
tb
# Liquidität, marginal (in kEUR) 
round(liquidity(ptf, tb, type="marginal", tree=Bilanz)/1000, 0)

# Einkommen, marginal (in kEUR)
round(income(ptf, tb, type="marginal", tree=Bilanz, revaluation.gains=FALSE)/1000, 0)

# Nominalwert (in kEUR)
round(value(ptf, tb, type="nominal", tree=Bilanz)/1000, 0)

# Liquidität, cumulative (in kEUR) 
round(liquidity(ptf, tb, type="cumulative", tree=Bilanz)/1000, 0)

# Einkommen, cumulative (in kEUR)
round(income(ptf, tb, type="cumulative", tree=Bilanz, revaluation.gains=FALSE)/1000, 0)

# Marktnaher Wert (in kEUR)
round(value(ptf, tb, type="markToModel", method=diskont, tree=Bilanz, digits=0)/1000, 0)

# Resultate aus momentaner Markumgebung
res <- round(value(ptf, by, type="markToModel", method=diskont, tree=Bilanz, digits=0)[1,]/1000, 0)

# Monte-Carlo Simulation
for(i in 1:100) {
# Zeige Fortschritt der Simulation
  require(svMisc)
  progress(i, progress.bar=TRUE) 

  # generiere neue Preise
  gas.p <- p.sim(10, tail(gas.annual, 1)$price, tail(gas.annual,1)$time,
            "1 years", gas.norm[1], gas.norm[2])
  gas.idx <- Index(
      MarketObjectCode = "GAS",
      Data = list(Dates=paste0(time(gas.p), "T00"), Values=series(gas.p)))
  el.p <- p.sim(10, tail(el.annual, 1)$price, tail(el.annual, 1)$time,
            "1 years", el.norm[1], el.norm[2])
  el.idx <- Index(
      MarketObjectCode = "EL",
      Data=list(Dates=paste0(time(el.p), "T00"), Values=series(el.p)))

  # generiere neue Marktumgebung
  rf <- RFConn(list(yc, gas.idx, el.idx))
  set(diskont, rf)
  set(ptf, rf)

  # berechne Marktnaher-Wert und speichere in Resultate-Struktur
  res <- rbind(
    res,
    round(value(ptf, by, type="markToModel", method=diskont, tree=Bilanz)[1,]/1000, 0))
}

# Dimension der Resultate?
dim(res)

# Verteilung Resultate
alpha <- 0.95
par(mfrow=c(3,2))
for(i in 1:ncol(res)) {
hist(res[,i], main=paste0("Marktnaher Wert zur Zeit: ", colnames(res)[i]))
var <- quantile(res[,i], 1-alpha)
abline(v=var, col="red")
abline(v=mean(res[res[,i]<=var,i]), col="blue")
}

