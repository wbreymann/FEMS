# Example for model

rm(list=ls())
devtools::load_all()
# library(data.tree)

myModel = ModelStructure("Minimal Model")
myModel
myModel$Active$Treasury$contracts

# Current account must be initialized


# addActive("Mortgages", myModel)
# myModel

class(myModel$Active)

# addContracts(list(CurrentAccount()), myModel$Active) # error because account is not a leaf
# addContracts(list(CurrentAccount()), myModel$Active$Mortgages)

# myModel$Active$Mortgages$contracts

# is.null(FindNode(myModel, "Mortgages"))

# addContracts(list(CurrentAccount()), FindNode(myModel, "Mortgages"))
# length(myModel$Active$Mortgages$contracts)

myModel


# Add Operations account
# define analysis time
ad="2016-01-01"
(times = timeSequence(from=timeDate(substring(ad, 1, 10)), by="1 years", 
                      length.out=5))
# Expenses for the daughter's studies:
# 4% of the current wealth
initialWealth <- 150000

ops.expenses = function(model, params) { 
  # For the example of the father who is paying for his daughter's studies,
  # we need here something that extracts the nominal value of the total 
  # wealth at the given dates.
  # I just put the initial value as dummy
  timeSeries( 
    rep(-initialWealth * 0.04, length(times)), 
    times
    )
}

#-----------------------------------------------------------------------------
# Modelling the expenses
# create Operations contract with "CashFlowPattern"
ops1 = Ops(ContractID="Ops001",
           Currency="CHF",
           CashFlowPattern = ops.expenses)

terms(ops1)
ops1$ContractType
ops1$CashFlowPattern
ops1$Params

# add contract to account Operations
addContracts(list(ops1), FindNode(myModel, "Operations"))
length(myModel$Operations$contracts)
myModel$Operations$contracts

# This defines the yield curve observed at the analysis date. 
yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc.ch <- YieldCurve(MarketObjectCode = "YC_CH", ReferenceDate = ad, 
                    Tenors = yc.tnr, Rates = yc.rts)
plot(yc.ch)
rf = RFConn(list(yc.ch))
rf
plot(ops.expenses(rf, "YC_CH"))
# link Operations contract with market environment
set(ops1, rf)

# pure events
# Start one day earlier
ad0 = as.character(timeDate(ad) - 1*24*3600)
ad0
events1 = events(ops1, ad0)
# print(events1)  # Error
events1

# liquidity
by <- times[1:5]- 1*24*3600
tb <- timeBuckets(by, bucketLabs=2016:2019)
tb
liquidity(ops1, by=tb, type="marginal")


# income
income(ops1, by=tb, type="marginal")  ## Error

# nominal value
value(ops1, by=by, type="nominal")

rf
rf[["YC_CH"]]

# Discount-Engine für die Barwertberechnung:
eng <- DcEngine(RiskFactorObjectLink=rf[["YC_CH"]])
set(eng, rf)

# Barwert der (erwarteten) operativen Cashflows
value(ops1, by=tb, type="markToModel", method=eng) ## Error
