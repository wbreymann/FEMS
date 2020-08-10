
rm(list=ls())
devtools::load_all()


#################################################################################################
# Example 1:
# set starting date and yield curve
t0 <- "2013-12-31"
(yc_flat <- FlatCurve2(0.03, t0))
yc_flat$MarketObjectCode <- "YC_FLAT"

# define the in- and out-flows
(cashflows <- data.frame(CashFlows = 150000, row.names = "2014-06-30"))
perc_out_dt <- c("2013-12-31","2014-12-31")
(percentage_outflows <- data.frame(PercentageOutflows = rep(0.04, length(perc_out_dt)), 
                                   row.names = perc_out_dt))

# construct current account
curr_acc <- CurrentAccount(ContractID = "Test_CurrAcc",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         CashFlows = cashflows,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "YC_FLAT")
curr_acc

# construct riskfactor connector
rf <- RFConn(yc_flat)

# calculate event series
# currently still not the same format as an rActus EventSeries
(evs.curr_acc <- events(curr_acc, "2012-12-31", rf, end_date="2018-12-31"))
(evs.curr_acc.1 <- events(curr_acc, "2012-12-31", rf, end_date="2013-12-31"))  ## Error: start/ end dates
(evs.curr_acc.2 <- events(curr_acc, "2013-12-31", rf, end_date="2014-12-31"))
(evs.curr_acc.3 <- events(curr_acc, "2014-12-31", rf, end_date="2015-12-31"))  ## Error: start date
(evs.curr_acc.4 <- events(curr_acc, "2015-12-31", rf, end_date="2016-12-31"))  ## Error: start date
(evs.curr_acc.5 <- events(curr_acc, "2016-12-31", rf, end_date="2017-12-31"))  ## Error: start date
(evs.curr_acc.6 <- events(curr_acc, "2017-12-31", rf, end_date="2018-12-31"))  ## Error: start date



#################################################################################################
# Example 2:
# Just a negative amount from the beginning...
# Interest payment currently not relevant since interest accrued is reflected in account value
t0 <- "2012-12-30"
(yc_flat <- FlatCurve2(0.03, t0))
yc_flat$MarketObjectCode <- "YC_FLAT"
rf <- RFConn(yc_flat)

(cashflows2 <- data.frame(CashFlows = -10000, row.names = "2013-12-31"))
curr_acc2 <- CurrentAccount(ContractID = "Test_CurrAcc2",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         CashFlows = cashflows2,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "YC_FLAT")
(evs.curr_acc2 <- events(curr_acc2, "2012-12-31", rf, end_date="2014-08-30"))
(evs.curr_acc2 <- events(curr_acc2, "2012-12-31", rf, end_date="2013-11-30"))  # Error: last CF later than end date.


#################################################################################################
# Example 3:
# Multiple cash in- and outflows...
(cashflows3 <- data.frame(CashFlows = c(10000, -2000, 4000, -1000), 
                          row.names = c("2013-12-31","2014-12-31","2015-12-31","2016-12-31")))
curr_acc3 <- CurrentAccount(ContractID = "Test_CurrAcc3",
                            ContractDealDate = t0,
                            Currency = "CHF",
                            CashFlows = cashflows3,
                            CycleAnchorDateOfInterestPayment = t0,
                            CycleOfInterestPayment = "1Y-",
                            MarketObjectCodeRateReset = "YC_FLAT",
                            Compound = "continuous")

(evs.curr_acc3 <- events(curr_acc3, "2014-12-31", rf, end_date="2020-12-31"))


#################################################################################################
# Example 4:
# example from the exercise...
t0 <- "2013-12-31"
(yc_flat <- FlatCurve2(0.03, t0))
yc_flat$MarketObjectCode <- "FlatCurve"
(cashflows <- data.frame(CashFlows = 150000, row.names = "2013-12-31"))
perc_out_dt <- c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31")
(percentage_outflows <- data.frame(PercentageOutflows = rep(0.04, length(perc_out_dt)), 
                                   row.names = perc_out_dt))
curr_acc <- CurrentAccount(ContractID = "CurrentAccount_1",
                         ContractDealDate = t0,
                         Currency = "CHF",
                         CashFlows = cashflows,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         MarketObjectCodeRateReset = "FlatCurve")

(ev <- events(curr_acc, "2013-12-31", RFConn(yc_flat), end_date="2018-12-31"))



