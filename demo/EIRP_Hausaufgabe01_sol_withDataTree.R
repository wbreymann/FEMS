
# Deaktiviere Warnungen temporär
options(warn=-1)
# Lade nötige Pakete
#library(rActus)
#library(rflPortfolio)
#library(rflSimulation)
# Definieren vom Analysezeitpunkt
t0 = "2016-01-02T00"	
# Importieren des Portfolios
ptf.tbl = read.csv("./FixedIncomePortfolio3.csv",header=TRUE)
ptf = Portfolio(source = ptf.tbl)
ptf
# Auflisten spezifischer Kontrakt-Parameter
pars=ptf.tbl[, c( "ContractID",
                  "ContractType",
                  "ContractRole",
                  "NotionalPrincipal",
                  "MaturityDate",
                  "PremiumDiscountAtIED",
                  "NominalInterestRate",
                  "CycleOfInterestPayment",
                  "CycleOfPrincipalRedemption",
                  "CycleOfRateReset")]

# Print Parameters with short column names
pars.pretty = pars
colnames(pars.pretty) = c("CID","CT","CNTRL","NT","MD","PDIED","IPNR","IPCL","PRCL","RRCL")
pars.pretty
# Definiere hierarchische Bilanzstruktur
fd.id = subset(pars, ContractRole=="RPA" & CycleOfRateReset=="NULL")$ContractID
vd.id = subset(pars, ContractRole=="RPA" & CycleOfRateReset!="NULL")$ContractID
ib.id = subset(pars, ContractRole=="RPL"&ContractType=="PAM")$ContractID
kk.id = subset(pars, ContractRole=="RPL"&ContractType%in%c("ANN","LAM"))$ContractID
Bilanz = Tree(list(
branches = list( 
  Bilanz = c("Aktiva","Passiva"),
  Aktiva = c("FixeDarlehen", "VariableDarlehen"),
  Passiva = c("Interbank","Kundenkonten")
),
leafs=list(
  FixeDarlehen = fd.id,
  VariableDarlehen = vd.id,
  Interbank = ib.id,
  Kundenkonten = kk.id
)
))
Bilanz
# Welche Marktobjekte referenziert?
obj = unique(ptf.tbl$MarketObjectCodeRateReset)
# -> single object "YC_EA_AAA"
obj = obj[which(obj!="NULL")] # "NULL" steht für undefiniert
obj

	# Definiere Marktumgebung per Analysezeitpunkt
	ir.1d = 0.03  # 1-day tenor
	ir.10y = 0.03  # 10-year tenor
yc <- YieldCurve(MarketObjectCode = obj, ReferenceDate = t0, 
             Tenors = c("1D", "10Y"), Rates = c(ir.1d, ir.10y))
plot(yc)
rf <- RFConn(yc)

# Verbinden von Portfolio und Marktumgebung
set(ptf,rf)

# Definieren der Diskontierungsmethode unter Verwendung 
# derselben Zinskurve wie oben und mit Spread von 0%
eng <- DcEngine(DiscountingSpread=0.0, RiskFactorObjectLink=obj, rf)

by = timeSequence(substring(t0,1,10),by="1 year",length.out=6)
years = as.character(2016:2020)
by 

tb = timeBuckets (by, bucketLabs=years)
tb

liquidity(ptf, by=tb, type="marginal", tree=Bilanz, digits=0)
income(ptf, by=tb, type="marginal", revaluation.gains=FALSE, tree=Bilanz, digits=0)
val = cbind(value(ptf, by=t0, type="nominal", tree=Bilanz, digits=0),
          value(ptf, by=t0, type="markToModel", method=eng, tree=Bilanz, digits=0))
colnames(val) = c("nominal", "markToModel")
print(paste0("Valuation on ", substring(t0,1,10),":\n"))
val

value(ptf, by=by, type="nominal", tree=Bilanz, digits=0)

# Eigenkapitalrate
(equity = round(value(ptf, by=t0, type="markToModel", method=eng, tree=Bilanz)["Bilanz",], 0))
(assets = round(value(ptf, by=t0, type="markToModel", method=eng, 
                      tree=Bilanz)["Bilanz.Aktiva",], 0))
cat(paste0("Eigenkapitalrate:\n ", round(100 * equity/assets,2), "%"))
# Definieren der Art des Neugeschäfts für jedes "Leaf"-Konto

#    Kundenkonten:
#    -> verwende existierenden Kontrakt im "Leaf"
kundenkonten.template = 
  get(get(ptf,as.character(Bilanz$leafs$Kundenkonten[1])),"all")

kundenkonten.template = 
  get(ptf[[Bilanz$leafs$Kundenkonten[1]]],"all")

#    Interbank:
#    -> verwende existierenden Kontrakt im "Leaf"
interbank.template = 
  get(get(ptf,as.character(Bilanz$leafs$Interbank[1])),"all")

#    Fixe Kredite:
#    -> verwende existierenden Kontrakt im "Leaf"
darlfix.template = 
  get(get(ptf,as.character(Bilanz$leafs$FixeDarlehen[1])),"all")

#    Variable Kredite:
#    -> verwende existierenden Kontrakt im "Leaf"
darlvar.template = 
  get(get(ptf,as.character(Bilanz$leafs$VariableDarlehen[1])),"all")

# -> combine template instruments
templates = list(
  Kundenkonten=kundenkonten.template,
  Interbank=interbank.template,
  FixeDarlehen=darlfix.template,
  VariableDarlehen=darlvar.template
)

# Definiere Wachstumsstrategie
strategie = diag(c(1.05, 1.05, 1.05, 1.05))
colnames(strategie) = c("Kundenkonten", "Interbank", "FixeDarlehen", "VariableDarlehen")
rownames(strategie) = colnames(strategie)
strategie
# Simulation über 5 Jahre

# Zeitstrahl für Simulation von oben

# Simuliere Neugeschäft
newcts = newbiz(ptf, Bilanz, by=tb, strategie, templates, rf)
# übersicht neu generierte Kontrakte
class(newcts)

vars = c( "ContractID", "ContractType", "ContractRole", "NotionalPrincipal", 
          "MaturityDate", "PremiumDiscountAtIED", "NominalInterestRate", 
          "NextPrincipalRedemptionPayment", "MarketObjectCodeRateReset")


newcts.vars = CTterms(newcts, vars=vars)
newcts.vars[,"ContractID"]
Bilanz
# Konto: Fixe Darlehen
fdar.sim = newcts.vars[grep("FixeDarlehen", newcts.vars[,"ContractID"]),]
fdar.sim

# Konto: Variable Darlehen
vdar.sim = newcts.vars[grep("VariableDarlehen", newcts.vars[,"ContractID"]),]
vdar.sim

# Konto: Interbank Darlehen
ib.sim = newcts.vars[grep("Interbank", newcts.vars[,"ContractID"]),]
ib.sim

# Konto: Kundenkonten
kk.sim = newcts.vars[grep("Kundenkonten", newcts.vars[,"ContractID"]),]
kk.sim

# "leafs" der Bilanz enthalten die neuen Kontrakte:
Bilanz$leafs
# Analyse der Bank-Strategie:
# -> Kombinieren von Start-Portfolio und simuliertem Neugeschäft
ptf.sim = c(ptf, newcts)
ptf.sim
set(ptf.sim, rf)
# Jährliches Wachstum
wert.nom = value(ptf.sim, by=tb, type="nominal", tree=Bilanz, digits=0)
round(wert.nom, 0)
wachstum = wert.nom[,-1]/wert.nom[,-ncol(wert.nom)]
wachstum
# -> Wachstum ist 5%, aber in Kundenkonten hat die Simulation einen Bug!!
apply(wachstum[c(3,4,6,7),], 2, all.equal, diag(strategie))
# Eigenkapitalquote als Funktion der Zeit
kapital.nom = value(ptf.sim, by=tb, type="nominal", tree=Bilanz)["Bilanz",]
kapital.nom/wert.nom["Bilanz.Aktiva",]
# Eigenkapitalquote auf "marktnaher Basis" als Funktion der Zeit
wert.markt = value(ptf.sim, by, type="markToModel", tree=Bilanz, method=eng, digits=0)
wert.markt
kapital.markt = wert.markt["Bilanz",]
kapital.markt/wert.markt["Bilanz.Aktiva",]
# Berechnung von Liquidität und Zinseinkommen
liq = liquidity(ptf.sim, by=tb, type="marginal", tree=Bilanz, digits=0)
inc = income(ptf.sim, by=tb, type="marginal", tree=Bilanz, revaluation.gains=FALSE, digits=0) 

################################################################################
# Erstellen einer Bilanz mit einem Kontokurrent, in dem die Liquiditätsdifferenzen
# verbucht werden
liq
inc
wert.nom
cc = numeric(6)
cc[1] = 0
for (i in 2:length(cc))
{
  cc[i] = cc[i-1] + liq["Bilanz",i-1]  
}
Bilanz.Aktiva.CurAccount = cc

wert.nom.ext = rbind(
  wert.nom[1:2,],
  Bilanz.Aktiva.CurAccount = cc,
  wert.nom[3:7,]
)
wert.nom.ext["Bilanz.Aktiva",] = colSums(wert.nom.ext[3:5,])
wert.nom.ext[1,] = colSums(wert.nom.ext[c(2,6),])
wert.nom.ext

# Jährlicher Wertzuwachs:
diff(as.numeric(wert.nom.ext["Bilanz",]))
# Der Zuwachs muss gerade dem "nominal income" entsprechen:
inc["Bilanz",]
# Bis auf Approximationsfehler stimmt dies auch:
diff(as.numeric(wert.nom.ext["Bilanz",])) - inc["Bilanz",]
################################################################################

# Beziehung zwischen der Differenz der Beiden Grössen 
# und dem Wachstum des Kontos „Bilanz“
# ??
# Bevorzugte Bewertungsmethode
# -> "freies" Kapital im Falle von Nominal-Bewertung
(kapital.nom/wert.nom["Bilanz.Aktiva",]-0.05)*wert.nom["Bilanz.Aktiva",]

# -> "freies" Kapital im Falle von Marktnaher-Bewertung
(kapital.markt/wert.markt["Bilanz.Aktiva",]-0.05)*wert.markt["Bilanz.Aktiva",]

# -> Marktnahe-Bewertung ist vorzuziehen
# Neue Strategie unter Annahme steigender Zinsen
s.zinsanstieg = diag(c(1.1, 1.0, 0.9, 1.1))
colnames(s.zinsanstieg) = 
  c("Kundenkonten", "Interbank", "FixeDarlehen", "VariableDarlehen")
rownames(s.zinsanstieg) = colnames(s.zinsanstieg)
s.zinsanstieg

# Srategie der Geschäftsfortführung
s.konst = diag(c(1.0, 1.0, 1.0, 1.0))
colnames(s.konst) = c("Kundenkonten", "Interbank", "FixeDarlehen", "VariableDarlehen")
rownames(s.konst) = colnames(s.konst)
s.konst

# Wieso "performt" s.zinsanstieg besser als s.konst?
# -> Bei steigendem Zinsniveau ist Neugeschäft unter variablen Zinsen vorzuziehen
#    um am steigenden Zinsniveau partizipieren zu können.
# Vergleich Resultate

# Definiere Marktumgebung
ir.1d = 0.001
ir.10y = 0.03
yc = YieldCurve(MarketObjectCode = obj, ReferenceDate = t0, 
              Tenors = c("1D", "10Y"), Rates = c(ir.1d, ir.10y))
plot(yc)
rf <- RFConn(yc)

# Verlinke Portfolio und Diskontierungs-Methode mit Marktumgebung
set(ptf, rf)
set(eng, rf)

# -> Neugeschäft "Steigende Zinsen"
# Zunächst alte "leafs" der Bilanz wiederherstellen!!!
Bilanz$leafs = list(
  FixeDarlehen = fd.id,
  VariableDarlehen = vd.id,
  Interbank = ib.id,
  Kundenkonten = kk.id
)

new.zinsanstieg = newbiz(ptf, Bilanz, by, s.zinsanstieg, templates,rf)
ptf.zinsanstieg = c(ptf, new.zinsanstieg)
set(ptf.zinsanstieg, rf)

# -> Neugeschäft "Fortführung"
Bilanz$leafs = list(
  FixeDarlehen = fd.id,
  VariableDarlehen = vd.id,
  Interbank = ib.id,
  Kundenkonten = kk.id
)
new.konst=newbiz(ptf, Bilanz, by, s.konst, templates,rf)
ptf.konst=c(ptf, new.konst)
set(ptf.konst, rf)

# -> Vergleich Nominal-Wert
rbind(value(ptf.konst, by, type="nominal", tree=Bilanz)["Bilanz",],
      value(ptf.zinsanstieg, by, type="nominal", tree=Bilanz)["Bilanz",])

# -> Vergleich Marktnaher-Wert
rbind(value(ptf.konst, by, type="markToModel", method=eng, tree=Bilanz)["Bilanz",],
      value(ptf.zinsanstieg, by, type="markToModel", method=eng, tree=Bilanz)["Bilanz",])

