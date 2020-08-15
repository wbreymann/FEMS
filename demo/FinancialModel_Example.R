# Example for FinancialModel

rm(list=ls())
devtools::load_all()

# Define analysis time
t0="2016-01-01"
ad0 = as.character(timeDate(t0) - 1*24*3600)
ad0

# Define risk factor environment
(yc.flat <- FlatCurve2(0.03, t0))
yc.flat$MarketObjectCode <- "YC_CH"

rf1 = RFConn(list(yc.flat))
rf1

# Initialize current account
CurrAcc.balance = 150000
cashflows_dt <- c("2016-12-31","2017-12-31","2018-12-31")
(cashflows <- data.frame(CashFlows = c(1000,-1000,2000), row.names = cashflows_dt))

curr_acc <- CurrentAccount(ContractID = "CurrAcc",
                           ContractDealDate = t0,
                           Currency = "CHF",
                           NotionalPrincipal = CurrAcc.balance,
                           CashFlows = cashflows,
                           CycleAnchorDateOfInterestPayment = t0,
                           CycleOfInterestPayment = "1Y-",
                           MarketObjectCodeRateReset = "YC_CH")
curr_acc


# Add Operations account
(times = timeSequence(from=timeDate(substring(t0, 1, 10)), by="1 years", 
                      length.out=5))
# Expenses for the daughter's studies:
# 4% of the current wealth
initialWealth <- 150000
ops.expenses = function(model, params) { 
  # For the example of the father who is paying for his daughter's studies,
  # we need here something that extracts the nominal value of the total 
  # wealth at the given dates.
  # I just put the initial value as dummy
  timeSeries(rep(-initialWealth * 0.04, length(times)), times)
}

#-----------------------------------------------------------------------------
# Modelling the expenses
# create Operations contract with "CashFlowPattern"
ops1 = Ops(ContractID="Ops001",
           Currency="CHF",
           CashFlowPattern = ops.expenses)

ops.expenses(rf1, "YC_CH")
# link Operations contract with market environment
set(ops1, rf1)

##########################################
# Create model structure
myModel = ModelStructure("Minimal Model", curAcc = curr_acc)
myModel
# The contract is there:
myModel$Active$Treasury$contracts

# add contract to account Operations
addContracts(list(ops1), FindNode(myModel, "Operations"))
length(myModel$Operations$contracts)
myModel$Operations$contracts[[1]]
# Prune empty branches
# Otherwise analytics will not work properly
Prune(myModel, function(x) (!isLeaf(x) || !is.null(x$contracts) ) )
myModel


# # define the strategy
# strategie = diag(c(1.05, 1.05, 1.05, 1.05))
# colnames(strategie) = c("Kundenkonten", "Interbank", "FixeDarlehen", "VariableDarlehen")
# rownames(strategie) = colnames(strategie)
# 
# 
# # define templates 
# templates = list(
#   Kundenkonten=get(ptf[[Bilanz$leafs$Kundenkonten[1]]],"all"),
#   Interbank= get(get(ptf,as.character(Bilanz$leafs$Interbank[1])),"all"),
#   FixeDarlehen=get(get(ptf,as.character(Bilanz$leafs$FixeDarlehen[1])),"all"),
#   VariableDarlehen=get(get(ptf,as.character(Bilanz$leafs$VariableDarlehen[1])),"all")
# )

by <- times[1:5]- 1*24*3600
tb <- timeBuckets(by, bucketLabs=2016:2019)
tb


##############################
# create the simulation manager
FM <- FinancialModel(ModelStructure = myModel,
                        RiskFactors = rf1,
                        TimeBuckets = tb,
                        SimulationSteps = as.character(tb)
                        # Strategy = strategie,
                        # Templates = templates
                        )

# simulate the strategy
simulate(sim.manager, start_date = "2012-12-31", end_date = "2020-12-31")


##########################
# Compute contract events
# single contrats
(evs.ca <- events(curr_acc, ad0, rf1, end_date="2019-12-31"))
(evs.ops <- events(ops1, ad0, rf1))

# The whole model
myModel$Do(fun=events.modelstructure, ad=ad0, model=rf1, end_date="2019-12-31")

events(myModel, ad=ad0, model=rf1, end_date="2019-12-31")
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









