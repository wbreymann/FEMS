rm(list=ls())
library(FEMS)


#################################################################################################
# Example 1:
# set starting date and yield curve
t0 <- "2013-12-31"
(yc_flat <- MarketInterestRate(0.03, t0, label = "YC_FLAT"))

# define the in- and out-flows
(ext.tas <- timeSeries(data = 150000, charvec = "2014-06-30"))
perc_out_dt <- c("2013-12-31","2014-12-31")
(percentage_outflows <- timeSeries(data = rep(0.04, length(perc_out_dt)), 
                                   charvec = perc_out_dt))

# construct current account
curr_acc <- CurrentAccount(ContractID = "Test_CurrAcc",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         Balance = 50000,
                         ExternalTransactions = ext.tas,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "YC_FLAT")
curr_acc
#plot(curr_acc, "2012-12-31", yc = yc_flat)
curr_acc_tst <- bankAccount(t0, balance = 50000, ext_transactions = ext.tas, 
                            perc_outflows = percentage_outflows, ir = 0.03, irFreq = "1 year")

# construct riskfactor connector
rf <- RFConn(yc_flat)

# calculate event series
# currently still not the same format as an rActus EventSeries
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date="2018-12-31"))
# (evs.curr_acc <- events(curr_acc, "2012-12-31", rf)) # must produce an error
(evs.curr_acc.1 <- events(curr_acc, "2012-12-31", rf, end_date="2013-12-31"))  
(evs.curr_acc.2 <- events(curr_acc, "2013-12-31", rf, end_date="2014-12-31"))
(evs.curr_acc.3 <- events(curr_acc, "2014-12-31", rf, end_date="2015-12-31"))  
(evs.curr_acc.4 <- events(curr_acc, "2015-12-31", rf, end_date="2016-12-31"))  
(evs.curr_acc.5 <- events(curr_acc, "2016-12-31", rf, end_date="2017-12-31"))  
(evs.curr_acc.6 <- events(curr_acc, "2017-12-31", rf, end_date="2018-12-31"))  

# first add a single internal cash flow
add.internaltransfer(curr_acc, 
             timeSeries(data = 5000, charvec = "2019-06-30"))
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date="2019-06-30"))

# check liquidity function...
set(curr_acc, rf)
by <- timeSequence(substring("2012-12-31",1,10), by = "1 year", length.out = 8)
(tb <- timeBuckets(by, bucketLabs=2013:2019))
liquidity(curr_acc, by = tb, type = "marginal")
liquidity(curr_acc, by = tb, type = "cumulative")


# now add another...
add.internaltransfer(curr_acc, 
                     timeSeries(data = -2000, charvec = "2019-12-31"))
(evs.curr_acc_new <- events(curr_acc, "2012-12-31", rf, end_date="2019-12-31"))

evs = evs.curr_acc
by = timeSequence("2013-01-01", "2018-01-01", by="year")
by = timeBuckets(by, bucketLabs=2013:2017)
by

liquidity(evs, by, "marginal", digits=0)
value(evs, by, "nominal", digits=0)


# now add another cash flow to the current account...
add.externaltransaction(curr_acc, 
             timeSeries(data = 1000, charvec = "2015-06-30"))
(evs.curr_acc_new <- events(curr_acc, "2012-12-31", rf, end_date="2018-12-31"))

#################################################################################################
# Example 2:
# Just a negative amount from the beginning...
# Interest payment currently not relevant since interest accrued is reflected in account value
t0 <- "2012-12-30"
(yc_flat <- MarketInterestRate(0.03, t0, label = "YC_FLAT"))
rf <- RFConn(yc_flat)

(cashflows2 <- timeSeries(data = -10000, charvec = "2013-12-31"))
curr_acc2 <- CurrentAccount(ContractID = "Test_CurrAcc2",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         ExternalTransactions = cashflows2,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "YC_FLAT")
(evs.curr_acc2 <- events(curr_acc2, "2012-12-31", rf, end_date="2014-08-30"))
(evs.curr_acc2 <- events(curr_acc2, "2012-12-31", rf, end_date="2013-11-30"))  # Error: last CF later than end date.


#################################################################################################
# Example 3:
# Multiple cash in- and outflows...
(cashflows3 <- timeSeries(data = c(10000, -2000, 4000, -1000), 
                          charvec = c("2013-12-31","2014-12-31","2015-12-31","2016-12-31")))
curr_acc3 <- CurrentAccount(ContractID = "Test_CurrAcc3",
                            ContractDealDate = t0,
                            Currency = "CHF",
                            ExternalTransactions = cashflows3,
                            CycleAnchorDateOfInterestPayment = t0,
                            CycleOfInterestPayment = "1Y-",
                            MarketObjectCodeRateReset = "YC_FLAT",
                            Compound = "continuous")

(evs.curr_acc3 <- events(curr_acc3, t0, rf, end_date="2020-12-31"))


#################################################################################################
# Example 4:
# example from the exercise...
t0 <- "2013-12-31"
(yc_flat <- MarketInterestRate(0.03, t0, label = "FlatCurve"))
(cashflows <- timeSeries(data = 150000, charvec = "2013-12-31"))
perc_out_dt <- c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31")
(percentage_outflows <- timeSeries(data = rep(0.04, length(perc_out_dt)), 
                                   charvec = perc_out_dt))
curr_acc <- CurrentAccount(ContractID = "CurrentAccount_1",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         ExternalTransactions = cashflows,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "FlatCurve")

(ev <- events(curr_acc, "2013-12-31", RFConn(yc_flat), end_date="2018-12-31"))

#################################################################################################
# Example 5:
# check some accruels 
t0 <- "2013-12-31"
(yc_flat <- MarketInterestRate(0.03, t0, label = "FlatCurve"))
cashflows_dt <- c("2013-12-31","2014-06-30","2014-12-31","2015-06-30","2015-12-31")
(cashflows <- timeSeries(data = c(10000,-1000,2000,-3000,-5000), charvec = cashflows_dt))
perc_out_dt <- c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31")
(percentage_outflows <- timeSeries(data = rep(0.04, length(perc_out_dt)), 
                                   charvec = perc_out_dt))
curr_acc5 <- CurrentAccount(ContractID = "CurrentAccount_1",
                           ContractDealDate = t0,
                           Currency = "CHF",
                           ExternalTransactions = cashflows,
                           PercentageOutflows = percentage_outflows,
                           CycleAnchorDateOfInterestPayment = t0,
                           CycleOfInterestPayment = "1Y-",
                           MarketObjectCodeRateReset = "FlatCurve")

(evs.curr_acc5 <- events(curr_acc5, "2013-12-31", RFConn(yc_flat), end_date="2018-03-31"))


#################################################################################################
# Example 6:
# check some accruels 
t0 <- "2013-12-31"
yc_flat <- MarketInterestRate(0.03, t0, label = "FlatCurve")

cashflows_dt <- c("2013-12-31","2014-12-31","2015-12-31")
(cashflows <- timeSeries(data = c(10000,-1000,2000), charvec = cashflows_dt))

curr_acc6 <- CurrentAccount(ContractID = "CurrentAccount_6",
                            ContractDealDate = t0,
                            Currency = "CHF",
                            ExternalTransactions = cashflows,
                            CycleAnchorDateOfInterestPayment = t0,
                            CycleOfInterestPayment = "1Y-",
                            MarketObjectCodeRateReset = "FlatCurve",
                            NominalInterestRate = 0.02)

(evs.curr_acc6 <- events(curr_acc6, "2013-12-31", RFConn(yc_flat), end_date="2018-03-31"))


yc_flat <- YieldCurve(label = "FlatCurve", 
                      ReferenceDate = t0,
                      Rates = c(0.03, 0.035, 0.0375, 0.04, 0.042, 0.05, 0.06),
                      Tenors = c("1W", "6M", "1Y", "5Y", "10Y", "50Y", "100Y"))
set(curr_acc6, list(CycleAnchorDateOfRateReset = t0,
                    CycleOfRateReset = "1Y-"))
(evs.curr_acc6 <- events(curr_acc6, "2013-12-31", RFConn(yc_flat), end_date="2018-03-31"))

#################################################################################################
# Example 7:
# check some accruels 
t0 <- "2012-06-30"
yc_flat <- MarketInterestRate(0.03, t0, label = "FlatCurve")

cashflows_dt <- c("2013-12-31","2014-12-31","2015-12-31")
(cashflows <- timeSeries(data = c(1000,-1000,2000), charvec = cashflows_dt))

curr_acc7 <- CurrentAccount(ContractID = "CurrentAccount_7",
                            ContractDealDate = t0,
                            Currency = "CHF",
                            ExternalTransactions = cashflows,
                            CycleAnchorDateOfInterestPayment = "2013-12-31",
                            CycleOfInterestPayment = "1Y-",
                            MarketObjectCodeRateReset = "FlatCurve",
                            Balance = 15000,
                            AccruedInterest = 120)

(evs.curr_acc7 <- events(curr_acc7, t0, RFConn(yc_flat), end_date="2016-03-31"))


