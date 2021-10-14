
# Deaktiviere Warnungen temporär
options(warn=-1)

# Lade nötige Pakete
library(FEMS)

# Definieren vom Analysezeitpunkt
t0 <- "2016-01-02"

# Importieren des Portfolios
ptf.tbl <- read.csv("./data/FixedIncomePortfolio3.csv", header = TRUE)
ptf <- Portfolio()
import(ptf,source = ptf.tbl)
ptf

# Auflisten spezifischer Kontrakt-Parameter
pars <- ptf.tbl[, c( "ContractID",
                  "ContractType",
                  "ContractRole",
                  "NotionalPrincipal",
                  "MaturityDate",
                  "PremiumDiscountAtIED",
                  "NominalInterestRate",
                  "CycleOfInterestPayment",
                  "NextPrincipalRedemptionPayment",
                  "CycleOfPrincipalRedemption",
                  "CycleOfRateReset")]

# Print Parameters with short column names
pars.pretty <- pars
colnames(pars.pretty) <- c("CID", "CT", "CNTRL", "NT", "MD", "P/D", "IR", 
                          "IPCL", "PR", "PRCL","RRCL")
pars.pretty

# 3. Definieren hierarchische Bilanzstruktur
# Bilanz <- Node$new("Bilanz")
# Bilanz$AddChild("Assets")
# Bilanz$AddChild("Liabilities")
# Bilanz$Assets$AddChild("FixeDarlehen")
# Bilanz$Assets$AddChild("VariableDarlehen")
# Bilanz$Liabilities$AddChild("Interbank")
# Bilanz$Liabilities$AddChild("Kundenkonten")

Bilanz <- institution("BankA")
temp.node <- FindNode(Bilanz,"LongTerm")
temp.node$name <- "FixeDarlehen"
Bilanz$Assets$AddChild("VariableDarlehen")
Bilanz$Liabilities$AddChild("Interbank")
Bilanz$Liabilities$AddChild("Kundenkonten")

# Füge die Kontrakte den einzelnen Konten hinzu
fd.id <- subset(pars,ContractRole == "RPA" & CycleOfRateReset == "NULL")$ContractID
vd.id <- subset(pars,ContractRole == "RPA" & CycleOfRateReset != "NULL")$ContractID
ib.id <- subset(pars,ContractRole == "RPL" & ContractType == "PAM")$ContractID
kk.id <- subset(pars,ContractRole == "RPL" & ContractType %in% c("ANN","LAM"))$ContractID
fds <- get(ptf, fd.id)
vds <- get(ptf, vd.id)
ibs <- get(ptf, ib.id)
kks <- get(ptf, kk.id)
addContracts(fds, FindNode(Bilanz$Assets, "FixeDarlehen"))
addContracts(vds, FindNode(Bilanz$Assets, "VariableDarlehen"))
addContracts(ibs, FindNode(Bilanz$Liabilities, "Interbank"))
addContracts(kks, FindNode(Bilanz$Liabilities, "Kundenkonten"))

# 4. Welche Marktobjekte referenziert?
obj <- unique(ptf.tbl$MarketObjectCodeOfRateReset)
# -> single object "YC_EA_AAA"
obj <- obj[which(obj != "NULL")] # "NULL" steht für undefiniert
obj

# Definiere Marktumgebung per Analysezeitpunkt
ir.1d <- 0.03  # 1-day tenor
ir.10y <- 0.03  # 10-year tenor
yc <- YieldCurve(label = obj,
                 ReferenceDate = t0, 
                 Tenors = c("1D", "10Y"), 
                 Rates = c(ir.1d, ir.10y))
rf <- RFConn()
add(rf, yc)
plot(yc)

# Verbinden von Portfolio und Marktumgebung
set(ptf,rf)

# Definieren der Diskontierungsmethode unter Verwendung 
# derselben Zinskurve wie oben und mit Spread von 0%
eng <- DcEngine()
set(eng, what = list(dc.spread = 0.0,
                     dc.object = yc))
set(eng, rf)

# Die folgenden Zeilen sind nicht verlangt
# Simuliere run-off
by <- timeSequence(t0, by = "1 year", length.out=6)
years <- as.character(2016:2020)
tb <- timeBuckets (by, bucketLabs = years)

events(Bilanz, t0, rf, end_date = "2025-12-31")
value(Bilanz, tb, type = "nominal")
7000 - (1000-0.01*5000 + 2*200)

# added liquidity, income and MtM value
liquidity(Bilanz, by = tb, type = "marginal", digits = 0)
income(Bilanz, by = tb, type = "marginal", revaluation.gains = FALSE, digits = 0)


# 5. Definition der Art des Neugeschäfts für jedes "Leaf"-Konto

#    Kundenkonten:
#    -> verwende existierenden Kontrakt im "Leaf"
kundenkonten.template <- Bilanz$Liabilities$Kundenkonten$contracts[[1]]


#    Interbank:
#    -> verwende existierenden Kontrakt im "Leaf"
interbank.template <- Bilanz$Liabilities$Interbank$contracts[[1]]

#    Fixe Kredite:
#    -> verwende existierenden Kontrakt im "Leaf"
darlfix.template <- Bilanz$Assets$FixeDarlehen$contracts[[1]]

#    Variable Kredite:
#    -> verwende existierenden Kontrakt im "Leaf"
darlvar.template <- Bilanz$Assets$VariableDarlehen$contracts[[1]]

# -> combine template instruments
templates <- list(Kundenkonten = kundenkonten.template,
                  Interbank = interbank.template,
                  FixeDarlehen = darlfix.template,
                  VariableDarlehen = darlvar.template)

# 6. Definiere Wachstumsstrategie
strategie <- diag(c(1.05,1.05,1.05,1.05))
colnames(strategie) <- c("Kundenkonten","Interbank","FixeDarlehen","VariableDarlehen")
rownames(strategie) <- colnames(strategie)
strategie

# 7. Simulation über 5 Jahre

# Zeitstrahl für Simulation
by <- timeSequence(t0, by = "1 year", length.out = 6)

# Simuliere Neugeschäft
newcts <- newbiz(Bilanz, by, strategie, templates, rf)
# übersicht neu generierte Kontrakte

# 8. Auflisten der neu-generierten Kontrakte
# Konto: Fixe Darlehen
fdar.sim <- as.data.frame(
  t(sapply(get(newcts,"contracts")[["FixeDarlehen"]],
           get,what=c( "ContractID",
                       "ContractType",
                       "ContractRole",
                       "NotionalPrincipal",
                       "MaturityDate",
                       "PremiumDiscountAtIED",
                       "NominalInterestRate",
                       "NextPrincipalRedemptionPayment",
                       "MarketObjectCodeOfRateReset"))))
fdar.sim


# Test der Werte: Abschätzung des am 2.1.17 zu erzeugenden Volumens:
# Ich wess nicht, ob der Wert exakt ist oder eine obere Schranke.
# Hängt davon ab, ob NextPrincipalRedemption beim ANN wirklich nur die Tilgung
# meint oder auch den Anteil Zinszahlung einschliess.
# Jedenfalls ist das neu erzeugte Volumen deutlich höher.
# 
Bilanz
pars.pretty[c(1,2,3,5,11,12,13,14),]
sum(pars.pretty[c(1,2,3,5,11,12,13,14),"NT"])*0.05 + 6000 + 12*300 + 1000

# Konto: Variable Darlehen
vdar.sim <- as.data.frame(
  t(sapply(get(newcts,"contracts")[["VariableDarlehen"]],
         get,what=c( "ContractID",
                     "ContractType",
                     "ContractRole",
                     "NotionalPrincipal",
                     "MaturityDate",
                     "PremiumDiscountAtIED",
                     "NominalInterestRate",
                     "NextPrincipalRedemptionPayment",
                     "MarketObjectCodeOfRateReset"))))




# Konto: Interbank Darlehen
ib.sim <- as.data.frame(
  t(sapply(get(newcts,"contracts")[["Interbank"]],
           get,what=c( "ContractID",
                       "ContractType",
                       "ContractRole",
                       "NotionalPrincipal",
                       "MaturityDate",
                       "PremiumDiscountAtIED",
                       "NominalInterestRate",
                       "NextPrincipalRedemptionPayment",
                       "MarketObjectCodeOfRateReset"))))



# Konto: Kundenkonten
kk.sim <- as.data.frame(
  t(sapply(get(newcts,"contracts")[["Kundenkonten"]],
           get,what=c( "ContractID",
                       "ContractType",
                       "ContractRole",
                       "NotionalPrincipal",
                       "MaturityDate",
                       "PremiumDiscountAtIED",
                       "NominalInterestRate",
                       "NextPrincipalRedemptionPayment",
                       "MarketObjectCodeOfRateReset"))))
kk.sim

Bilanz
pars.pretty[c(15,19,20),]
sum(pars.pretty[c(15,19,20),"NT"])
sum(pars.pretty[c(15,19,20),"NT"])*0.05 + 1000-0.01*5000 + 2*200
7000 -1700 + (1000-0.01*5000 + 2*200)


# 9. Analyse der Bank-Strategie:
# -> Kombinieren von Start-Portfolio und simuliertem Neugeschäft
bilanz.sim <- add.model(Bilanz, newcts)
events(bilanz.sim, t0, rf, end_date = "2025-12-31")

# Jährliches Wachstum
wert.nom <- value(bilanz.sim, tb, type="nominal")
wert.nom
wachstum=wert.nom[,-1]/wert.nom[,-ncol(wert.nom)]
wachstum

# -> Wachstum ist 5%
apply(wachstum[c(3,4,6,7),],2,all.equal,diag(strategie))

# Eigenkapitalquote als Funktion der Zeit
kapital.nom = value(bilanz.sim, tb, type="nominal")["1 Bilanz                  ",]
kapital.nom/wert.nom["2  ¦--Assets              ",]

# Eigenkapitalquote auf "marktnaher Basis" als Funktion der Zeit
wert.markt = value(bilanz.sim,tb,type="market",method=eng)
wert.markt
kapital.markt=wert.markt["1 Bilanz                  ",]
kapital.markt/wert.markt["2  ¦--Assets              ",]

# Berechnung von Liquidität und Zinseinkommen
liq=liquidity(bilanz.sim,tb,type="marginal")
inc=income(bilanz.sim,tb,type="marginal",revaluation.gains=FALSE)

# Beziehung zwischen der Differenz der Beiden Grössen 
# und dem Wachstum des Kontos „Bilanz“
# ??
# Bevorzugte Bewertungsmethode
# -> "freies" Kapital im Falle von Nominal-Bewertung
(kapital.nom/wert.nom["2  ¦--Assets              ",]-0.05)*wert.nom["2  ¦--Assets              ",]

# -> "freies" Kapital im Falle von Marktnaher-Bewertung
(kapital.markt/wert.markt["2  ¦--Assets              ",]-0.05)*wert.markt["2  ¦--Assets              ",]

# -> Marktnahe-Bewertung ist vorzuziehen
# Neue Strategie unter Annahme steigender Zinsen
s.zinsanstieg=diag(c(1.1,1.0,0.9,1.1))
colnames(s.zinsanstieg)=c("Kundenkonten","Interbank","FixeDarlehen","VariableDarlehen")
rownames(s.zinsanstieg)=colnames(s.zinsanstieg)
s.zinsanstieg

# Srategie der Geschäftsfortführung
s.konst=diag(c(1.0,1.0,1.0,1.0))
colnames(s.konst)=c("Kundenkonten","Interbank","FixeDarlehen","VariableDarlehen")
rownames(s.konst)=colnames(s.konst)
s.konst

# Wieso "performt" s.zinsanstieg besser als s.konst?
# -> Bei steigendem Zinsniveau ist Neugeschäft unter variablen Zinsen vorzuziehen
#    um am steigenden Zinsniveau partizipieren zu können.
# Vergleich Resultate

# Definiere Marktumgebung
ir.1d=0.001
ir.10y=0.03
yc <- YieldCurve(label = obj,
                 ReferenceDate = t0, 
                 Tenors = c("1D", "10Y"), 
                 Rates = c(ir.1d, ir.10y))
rf <- RFConn()
add(rf,list(yc))
plot(yc)

# Verlinke Portfolio und Diskontierungs-Methode mit Marktumgebung
set(ptf,rf)
set(eng,rf)

# -> Neugeschäft "Steigende Zinsen"
new.zinsanstieg <- newbiz(Bilanz,by,s.zinsanstieg,templates,rf)
ptf.zinsanstieg <- add.model(Bilanz, new.zinsanstieg)

# -> Neugeschäft "Fortführung"
new.konst <- newbiz(Bilanz,by,s.konst,templates,rf)
ptf.konst <- add.model(Bilanz, new.konst)

# -> Vergleich Nominal-Wert
rbind(value(ptf.konst,tb,type="nominal")["1 Bilanz                  ",],
      value(ptf.zinsanstieg,tb,type="nominal")["1 Bilanz                  ",])

# -> Vergleich Marktnaher-Wert
rbind(value(ptf.konst,tb,type="market",method=eng)["1 Bilanz                  ",],
      value(ptf.zinsanstieg,tb,type="market",method=eng)["1 Bilanz                  ",])

# Was ist mit Income und Liquidity?


