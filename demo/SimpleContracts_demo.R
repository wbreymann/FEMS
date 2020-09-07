#[17:15] Auth Christoph (auth)

# library(FEMS)
rm(list=ls())
devtools::load_all()

#------------------------------- Pure cash flows -------------------------------
# 1. Current account

# set starting date and yield curve
t0 <- "2013-12-31"
<<<<<<< HEAD
(marketRate <- MarketInterestRate(0.03, t0, label = "IR_MARKET"))
=======
(yc_flat <- MarketInterestRate(0.03, t0, label = "Rates_CH"))
>>>>>>> 4f9f530bea93cb7a7e74437e62e39e6fbcfd6ac3

# define the in- and out-flows
dates <- as.character(timeSequence(from = "2019-01-31", by = "month", length.out = 12))
(ext_tas <- timeSeries(data = rep(5000, 12), 
                       charvec = dates, 
                       units = "Ext.Transactions"))
# perc_out_dt <- c("2013-12-31","2014-12-31")
# (percentage_outflows <- timeSeries(data = rep(0.04, length(perc_out_dt)),
#                                    charvec = perc_out_dt,
#                                    units = "PercentageOutflows"))

curr_acc <- CurrentAccount(ContractID = "Test_CurrAcc",
                           ContractDealDate = t0,
                           Currency = "CHF",
                           Balance = 50000,
                           ExternalTransactions = ext_tas,
                           CycleAnchorDateOfInterestPayment = t0,
                           CycleOfInterestPayment = "1Y-",
                           MarketObjectCodeRateReset = "Rates_CH",
                           NominalInterestRate = 0.02,
                           CycleAnchorDateOfRateReset = "2014-12-31",
                           CycleOfRateReset = "P1YL1")

# or alternatively
(curr_acc <- bankAccount(t0, balance = 50000, ir = 0.02, ext_transactions = ext_tas,
                        MarketObjectCodeRateReset = "Rates_CH"))

# construct riskfactor connector
rf <- RFConn(yc_flat)

# calculate event series
# currently still not the same format as an rActus EventSeries
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date = "2019-12-31")) ## Error: CF not taken into account properly
cashFlows(curr_acc, from = "2012-12-31", riskfactors = rf) 
cashFlows(curr_acc, from = "2012-12-31", to = "2019-12-31", riskfactors = rf) 
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf)) # must produce an error
(evs.curr_acc.1 <- events(curr_acc, "2012-12-31", rf, end_date = "2013-12-31"))  
(evs.curr_acc.2 <- events(curr_acc, "2013-12-31", rf, end_date = "2014-12-31"))
(evs.curr_acc.3 <- events(curr_acc, "2014-12-31", rf, end_date = "2015-12-31"))  
(evs.curr_acc.4 <- events(curr_acc, "2015-12-31", rf, end_date = "2016-12-31"))  
(evs.curr_acc.5 <- events(curr_acc, "2016-12-31", rf, end_date = "2017-12-31"))  
(evs.curr_acc.6 <- events(curr_acc, "2017-12-31", rf, end_date = "2018-12-31"))  
(evs.curr_acc.6 <- events(curr_acc, "2018-12-31", rf, end_date = "2019-12-31"))  

# or directly only via YieldCurve
(evs.curr_acc <- events(curr_acc, "2012-12-31", yc_flat, end_date = "2019-12-31"))

# set different external transactions and internal transfers
(ext_tas <- timeSeries(data = rep(10000, 12), 
                       charvec = dates, 
                       units = "Ext.Transactions"))
set(curr_acc, ExternalTransactions = ext_tas)
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date = "2019-12-31"))


(int_tfs <- timeSeries(data = rep(-1000, 12), 
                       charvec = dates, 
                       units = "Int.Transfers"))
set(curr_acc, InternalTransfers = int_tfs)
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date = "2019-12-31"))

# add a single internal transfer
add.internaltransfer(curr_acc, 
                     (int_tfs <- timeSeries(data = 2000, 
                                            charvec = "2020-01-31", 
                                            units = "Int.Transfers")))
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date = "2020-01-31"))

#plot(curr_acc, "2012-12-31", yc = yc_flat)
#------------ Operational cash flows defined by an internal model -------------

# define analysis time
ad <- "2016-01-01"
(times <- timeSequence(from = ad, by = "1 months", length.out = 24))

# Define prices externally, here just as an initial value with
# normally distributed random increments
values <- cumsum(c(1,0.01*rnorm(23)))
idx <- Index(MarketObjectCode = "PriceIndex",
             Data = list(Dates = times, 
                         Values = values))
plot(idx)
# price.ts <- timeSeries(data = values, charvec = times)
# plot(price.ts)
# 
# This market interest rates
(yc.flat <- MarketInterestRate(0.03, ad, label = "Rates_CH"))

# yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
# yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
# yc.ch <- YieldCurve(MarketObjectCode = "YC_CH", ReferenceDate = ad, 
#                     Tenors = yc.tnr, Rates = yc.rts)
# plot(yc.ch)
plot(yc.flat)
rf1 <- RFConn(yc.flat, idx)
rf1
rf1[["PriceIndex"]]
rf1[["Rates_CH"]]

# Function that computes costs or revenues from prices
ops.profit <- function(model, idx, times) { 
  model[[idx]]$Data[times,] * 1000
}

#-----------------------------------------------------------------------------
# Modelling of operational revenues and expenses (Betriebskosten und -erträge)
# create Operations contract with "CashFlowPattern"
# ATTENTION: if a variable in CashFlowParams is not defined and happens to be
# the same as the one of an object available in the session, this on gets allocated
# and will produce an error.
# 
ops1 <- OperationalCF(ContractID="Ops001", Currency="CHF",
                  CashFlowPattern = ops.profit,
                  CashFlowParams = list(model = rf1, idx = "PriceIndex", 
                                        times = as.character(times)))

# Contract terms
terms(ops1)
ops1$ContractType
ops1$CashFlowPattern
ops1$CashFlowParams 


cfPattern <- do.call("ops.profit", ops1$CashFlowParams)
plot(cfPattern)

# link Operations contract with market environment
set(ops1, rf1)
# pure events
cashFlows(ops1, ad)
cashFlows(ops1, "2015-12-31")
events1 <- events(ops1, ad)
print(events1$evs)
plot(ops1, ad)  ## Doesn't work. 


#------------------------Investment with depreciation --------------------------

# Define the depreciation (linear oveer 24 months)
deprec <- function(times) {
  timeSeries(seq(1000, 0, length.out=24), times)
}

# Testing the function 
cfPattern <- do.call("deprec", list(times))
plot(cfPattern)

# Constructing the object
ops2 <- Investments(ContractID = "Ops002", Currency = "CHF", 
                    InvestPattern = deprec, InvestParams = list(times = times))


# Cashflows and more
cashFlows(ops2, ad)
cashFlows(ops2, "2015-12-31") # One day earlier. Look's good.
# Notice that here the depreciation is shown but it is not a cash flow!
# This can be distinguised here:
events2 <- events(ops2, ad)
events2 <- events(ops2, "2015-12-31")
print(events2)  # pretty print funktioniert nicht.
plot(ops2, ad)  # Doesn't work


#------------------------------------ Bonds ------------------------------------
b0 <- bond("2020-01-01")
cashFlows(b0)
plot(b0, "2020-01-01")

# I'm not yet very happy with "dealDate".
# Why not just "start"?
# We also need a currency (no default).
b1 <- bond(start = "2020-01-01", maturity = "5 years", nominal = 10000, coupon = 0.05)
cashFlows(b1)

# The function "events" can also be used, as for ACTUS CTs:
events(b1, "2020-01-01")

plot(b1, "2020-01-01")
plot(b1, "2020-01-02") # Ohne den ersten Cash-out

#----------------------------------- Annuity -----------------------------------
a0 <- annuity("2020-01-01")
cashFlows(a0)
plot(a0, "2020-01-01")

a1 <- annuity("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years")
cashFlows(a1)  
## Error: 
#   1. Höhe der jährl. Zahlung sollte aus der Maturität berechnet werden
#   2. Variable "Time" liefert doppelten Wert. Darf im Gegensatz zu den CFs nicht aggregiert werden.
#   3. Erste Zinszahlung fehlt
events(a1, "2020-01-01")
plot(a1, "2020-01-01")

a2 <- annuity("2020-01-01", nominal = 10000, ir = 0.05, annuity = 2000, maturity = "5 years")
cashFlows(a2)
plot(a2, "2020-01-01")


#----------------------- Loan with constant amortization -----------------------
b0 <- loan("2020-01-01")
cashFlows(b0)

b1 <- loan("2020-01-01", maturity = "5 years", nominal = 10000, ir = 0.05)
cashFlows(b1) # Error: Amount of amortization should be computed from maturity. Time in output wrong
events(b1, "2020-01-01")

b2 <- loan("2020-01-01", nominal = 10000, ir = 0.05, amort = 1000)
cashFlows(b2) # Error: Should compute maturity from mount of amortization

b3 <- loan("2020-01-01", maturity = "5 years", nominal = 10000, ir = 0.05, amort = 1000)
cashFlows(b3) # Error: Should compute maturity from mount of ammortization



