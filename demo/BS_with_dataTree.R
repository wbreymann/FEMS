options(warn=-1)
library(rActus)
library(rflPortfolio)
library(data.tree)
source("./R/DataTreeFunctions.R")

t0="2016-01-02T00"	
ptf=Portfolio(source="./FixedIncomePortfolio2.csv")
summary(ptf)
obj = unique(CTterms(ptf, vars="MarketObjectCodeRateReset"))
obj = obj[which(obj!="NULL"),1] # "NULL" steht für undefiniert
ir.1d = 0.001  # 1-day tenor
ir.10y = 0.03  # 10-year tenor
yc <- YieldCurve(MarketObjectCode = obj, ReferenceDate = t0, 
                 Tenors = c("1D", "10Y"), Rates = c(ir.1d, ir.10y))
rf <- RFConn(yc)
set(ptf,rf)
eng <- DcEngine(DiscountingSpread=0.0, RiskFactorObjectLink=obj, rf)
evL = events(ptf, t0)

# Anylytics
by = timeSequence(timeDate(substring(t0, 1, 10)), timeDate("2028-01-03"), by="1 year")
by
(tb = timeBuckets(t0=as.character(by), bucketLabs=2016:2027, breakLabs=paste0("02.01.",16:28)))

# tb@breakLabs
liquidity(ptf, by=tb, type="marginal", digits=0)
income(ptf, by=tb, type="marginal", revaluation.gains=FALSE, digits=0)
val = c(value(ptf, by=t0, type="nominal", digits=2),
        value(ptf, by=t0, type="markToModel", method=eng, digits=0))
names(val) = c("nominal", "markToModel")
print(paste0("Valuation on ", substring(t0,1,10),":"))
val

# Analysis Tree
Bilanz = Tree(list(branches = list(
  Bilanz = c("Aktiva", "Passiva"), 
  Aktiva = c("Kredite", "Hypotheken"), 
  Passiva = c("Interbank", "Kundenkonten")
)))

# Select contracts for leaf accounts:
pars = CTterms(ptf, vars = c("ContractID", "ContractRole", "ContractType"))
ct.kredite = subset(pars, ContractRole=="RPA" & ContractType=="PAM")$ContractID
ct.hyp = subset(pars, ContractRole=="RPA" & ContractType %in% c("ANN", "LAM"))$ContractID
ct.interbank = subset(pars, ContractRole=="RPL"&ContractType=="PAM")$ContractID
ct.kunden = subset(pars, ContractRole=="RPL"&ContractType%in%c("ANN", "LAM"))$ContractID
Bilanz$leafs = list(Kredite = ct.kredite, 
                    Hypotheken = ct.hyp, 
                    Interbank = ct.interbank, 
                    Kundenkonten = ct.kunden)

Bilanz
ll = Tree.to.nested.list(Bilanz)
names(ll$Aktiva$Kredite)
ll

# Create a tree of class Node as defined in the package data.tree
BS = FromListSimple(Tree.to.nested.list(Bilanz), nodeName="BS")
BS
BS$Aktiva$Hypotheken$ctNames
BS$totalCount
BS$Aktiva$Kredite$ctNames
plot(BS)

# Examples of how to work with data.tree objects
BS$Get("ctNames", filterFun = isLeaf)
aBS.leafs = Traverse(BS, traversal="pre-order", filterFun=isLeaf)
Get(aBS.leafs, "ctNames")

Traverse(BS, traversal="pre-order")
Traverse(BS, traversal="post-order")
Traverse(BS, traversal="in-order")
Traverse(BS, traversal="level")
Traverse(BS, traversal="ancestor")
Traverse(BS$Aktiva$Hypotheken, traversal="ancestor")

# Add node for Treasury account
BS$Aktiva$AddChild("Treasury")
BS
plot(BS)

# Before computing an analytics, the values should be cleared.
clearAnalytics(BS, "liquidity")
BS$Do(fun=fAnalytics, "liquidity", ptf, by=tb, type="marginal", filterFun=isLeaf)
BS$Aktiva$Treasury[["liquidity"]] = rep(0, length=length(BS$Aktiva$Kredite$liquidity))
aggregateAnalytics(BS, "liquidity")

# The following should be packaged into a "print" function:
liq = data.frame (t(BS$Get("liquidity", format = function(x) ff(x,0))), 
                  check.names=FALSE, fix.empty.names=FALSE)
rownames(liq) = capture.output(print(BS))[-1]
liq
# Note that there is no additional cash flow in the treasury account 
# because this is only a "pass-through"

# The same for income:
clearAnalytics(BS, "income")
BS$Do(fun=fAnalytics, "income", ptf, by=tb, type="marginal", 
      revaluation.gains=TRUE, method=eng, filterFun=isLeaf)
BS$Aktiva$Treasury$income = rep(0, length=length(BS$Aktiva$Kredite$liquidity))
aggregateAnalytics(BS, "income")
inc = data.frame (t(BS$Get("income", format = function(x) ff(x,0))), 
                  check.names=FALSE, fix.empty.names=FALSE)
rownames(inc) = capture.output(print(BS))[-1]
inc
#  No income for "Treasury"

# And now for value:
clearAnalytics(BS, "value")
BS$Do(fun=fAnalytics, "value", ptf, by=tb, type="markToMarket", method=eng, 
      filterFun=isLeaf)
BS$Aktiva$Treasury$value = rep(0,13)
BS$Aktiva$Treasury$value = cumsum(c(0, BS$liquidity))
aggregateAnalytics(BS, "value")
# BS$Aktiva$Kredite$value

val = data.frame (t(BS$Get("value", format = function(x) ff(x,0))), 
                  check.names=FALSE, fix.empty.names=FALSE)
rownames(val) = capture.output(print(BS))[-1]
val.without.treasury = val
val.with.treasury = val
# 
val.with.treasury
val.without.treasury


