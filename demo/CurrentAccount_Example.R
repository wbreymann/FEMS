
rm(list=ls())
devtools::load_all()


# t0 <- "2013-12-31"
# (yc_flat <- FlatCurve2(0.03, t0))
# ca_tst <- CurrentAccount(ContractID = "Test",
#                          ContractDealDate = t0,
#                          EndDate = "2018-12-31",
#                          Currency = "CHF",
#                          NotionalPrincipal = 150000,
#                          CycleAnchorDateOfEvents = t0,
#                          CycleOfEvents = "1Y-",
#                          Outflow = 0.04,
#                          YieldCurve = yc_flat)
# evs <- events(ca_tst, "2012-12-31")

# set starting date and yield curve
t0 <- "2013-12-31"
(yc_flat <- FlatCurve2(0.03, t0))

# define the in- and out-flows
(inflows <- data.frame(Inflows = 150000, row.names = "2014-06-30"))
(outflows <- data.frame(Outflows = 10000, row.names = "2014-06-30"))
perc_out_dt <- c("2013-12-31","2014-12-31")
(percentage_outflows <- data.frame(PercentageOutflows = rep(0.04, length(perc_out_dt)), 
                                   row.names = perc_out_dt))

# construct current account
ca_tst <- CurrentAccount(ContractID = "Test",
                         ContractDealDate = t0,
                         EndDate = "2015-12-31",
                         Currency = "CHF",
                         Inflows = inflows,
                         Outflows = outflows,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         YieldCurve = yc_flat)

# calculate event series
# currently not the same format as an rActus EventSeries
(evs_ca1 <- events(ca_tst, "2012-12-31"))

# example from ecercise...
t0 <- "2013-12-31"
(yc_flat <- FlatCurve2(0.03, t0))
(inflows <- data.frame(Inflows = 150000, row.names = "2013-12-31"))
perc_out_dt <- c("2013-12-31","2014-12-31","2015-12-31","2016-12-31","2017-12-31")
(percentage_outflows <- data.frame(PercentageOutflows = rep(0.04, length(perc_out_dt)), 
                                   row.names = perc_out_dt))
curr_acc <- CurrentAccount(ContractID = "CurrentAccount_1",
                         ContractDealDate = t0,
                         EndDate = "2018-12-31",
                         Currency = "CHF",
                         Inflows = inflows,
                         PercentageOutflows = percentage_outflows,
                         CycleAnchorDateOfInterestPayment = t0,
                         CycleOfInterestPayment = "1Y-",
                         YieldCurve = yc_flat)

# currently not the same format as an rActus EventSeries
(evs_ca1 <- events(curr_acc, "2013-12-31"))



