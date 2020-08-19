# Example for FinancialModel

# Preparation -------------------------------------------
rm(list=ls())
devtools::load_all()

# Define model structure -------------------------------------------------------
Father <- Node$new("Father")
Father$AddChild("Wealth")
Father$AddChild("Expenses")
Father

# Set times --------------------------------------------------------------------
(t.start <- "2014-01-01")
(tb <- timeBuckets(timeSequence(t.start, by="year", length.out=6), bucketLabs=2014:2018))
(t.end <- "2019-01-01")

# Cash flows and value of the empty model---------------------------------------
liquidity(Father, tb, "marginal")
value(Father, tb, "nominal")

# Define current account -------------------------------------------------------
# The current account keeps track of father's wealth
# Initialize current account
CurrAcc.balance = 150000
curr_acc <- CurrentAccount(ContractID = "CurrAcc",
                           ContractDealDate = "2013-12-31",
                           Currency = "CHF",
                           NotionalPrincipal = CurrAcc.balance,
                           # CashFlows = cashflows,
                           CycleAnchorDateOfInterestPayment = "2013-12-31",
                           CycleOfInterestPayment = "1Y-",
                           MarketObjectCodeRateReset = "YC_CH")
curr_acc

# Add contract(s) to account Wealth
addContracts(list(CurrAcc=curr_acc), FindNode(Father, "Wealth"))
length(Father$Wealth$contracts)
Father$Wealth$contracts 

# Define risk factor environment -----------------------------------------------
(yc.flat <- FlatCurve(0.03, "2013-12-31"))
yc.flat$MarketObjectCode <- "YC_CH"
(rf1 = RFConn(list(yc.flat)))

# Wealth evolution without expenses --------------------------------------------
events(Father, t.start, rf1, end_date=t.end)
Father$Wealth$eventList
liquidity(Father, tb, "marginal")
value(Father, tb, "nominal")

# Modelling the expenses -------------------------------------------------------
# Expenses for the daughter's studies:
# 4% of the current wealth
# initialWealth <- 150000

# function for expense cash flow pattern
# The valiue of the cashflow depends on the nominal value of the Wealth at 
# the end of the previous year.
# Therefore, the value method must be evaluated accordingly.
t.cfs <- timeSequence("2013-12-31", "2017-12-31", "year")
(tb.cfs <- timeBuckets(t.cfs, bucketLabs = 2014:2017))
expenses4daughter <- function(modelName, tb, percentage) { 
  # For the example of the father who is paying for his daughter's studies,
  # we need here something that extracts the nominal value of the total 
  # wealth at the given dates.
  # I just put the initial value as dummy
  model <- eval(as.name(modelName))
  val <- value(model, tb, "nominal")[1,]
  # print("Expenses4daughter: value")
  # print(val)
  timeSeries(-as.numeric(val) * percentage, as.timeDate(tb) + 24*3600)
}

expenses4daughter("Father", tb.cfs, 0.04)

# create Operations contract Expenses with expense cash flow pattern
Expenses = Operations(ContractID="Ops001",
           Currency="CHF",
           CashFlowPattern = expenses4daughter,
           CashFlowParams = list(modelName="Father", tb=tb.cfs, percentage=0.04))
# link Expense contract with market environment
set(Expenses, rf1)  # Erforderlich?

# Add contract for expenses to account Expenses
addContracts(list(Expenses), FindNode(Father, "Expenses"))

# Wealth evolution without dynamic simulation ----------------------------------
# events(Expenses, ad=t.start, model=rf1)
# events(curr_acc, t.start, model=rf1, end_date=t.end)
# events(Father, ad=t.start, model=rf1, end_date=t.end)
events(Father, ad="2013-12-31", model=rf1, end_date=t.end)
Father$Expenses$eventList
Father$Wealth$eventList
liquidity(Father, tb, "marginal")
value(Father, tb, "nominal")
expenses4daughter("Father", tb.cfs, 0.04)
FEMS:::clearEvents(Father)
# events(Father$Wealth, ad=t.start, model=rf1, end_date=t.end)
# events(Father$Expenses, ad=t.start, model=rf1, end_date=t.end)
# Father$Expenses$contracts
# Father$Wealth$eventList
# Father$Expenses$eventList

# Notice that the account Welath doesn't take into account the outflows.

# Create Financial model ------------------------------

(tb0 = timeBuckets(timeSequence("2013-12-31", "2018-12-31", "year"), bucketLabs=2014:2018))

FM <- FinancialModel(
  mstructure = Father, treasury= FindNode(Father, "Wealth"), curr_acc=curr_acc, 
  ad0="2013-12-31", rf = rf1, buckets = tb0, steps = as.character(tb)
  # Strategy = strategie, Templates = templates
)

# Simulate the strategy --------------------------------
# FM$simulate(start = "2015-12-31", end = "2020-12-31", by="1 year")
Father$Wealth$contracts[[1]]$InternalCashFlows <- data.frame()
FM$simulate(t.start = t.start, t.end = t.end, by="1 year")

liquidity(Father, tb, "marginal")
value(Father, tb, "nominal")
liquidity(Father, tb0, "marginal")
value(Father, tb0, "nominal")


##########################
# Compute contract events
# single contrats
(evs.ca <- events(curr_acc, t.start, rf1, end_date="2019-12-31"))
(evs.ops <- events(ops1, t.start, rf1))

# The whole model
myModel$Do(fun=events.modelstructure, ad=t.start, model=rf1, end_date="2019-12-31")

events(myModel, ad=t.start, model=rf1, end_date="2019-12-31")
# Test that the events are there:
myModel$Active$Treasury$eventList
myModel$Operations$eventList

#######################
# Analytics
# liquidity
liquidity(evs.ops, by=tb, type="marginal")
liquidity(evs.ca, by=tb, type="marginal")

value(evs.ops, by=tb, type="nominal")
value(evs.ca, by=tb, type="nominal")

# Compute liquidity for whole model
liquidity(myModel, by=tb, type="marginal")
liquidity(myModel$Active, by=tb, type="marginal")
liquidity(myModel$Active$Treasury, by=tb, type="marginal")  ## Error! Why?

# Compute value for whole model
# Nominal value:
value(myModel, by=tb, type="nominal")

# Discount-Engine für die Barwertberechnung:
eng <- DcEngine(RiskFactorObject=rf1[["YC_CH"]])  ## Error!
# Barwert der (erwarteten) operativen Cashflows
value(ops1, by=tb, type="markToModel", method=eng) ## Error

as.character(tb)









