####################
### EZB Übungen  ###
####################

setwd("rActus")
library("devtools")
devtools::load_all()

## Übungsblatt 1
#---------------

########################################################################
# Ex 1
(bc1.1 <- BaseContract(Dates=c("2013-12-31","2023-12-31"),
                      CashFlows=c(-150000,192013)))
(yc_flat <- MarketInterestRate(0.02500017, "2013-12-31"))
(val <- value(bc1.1, by="2023-12-31", curve=yc_flat, compound="compound")) 

(bc1.2 <- BaseContract(Dates=c("2013-12-31","2023-12-31"),
                      CashFlows=c(-150000,250000)))

# (a)
(sol_1.a <- solve(method="compound", period=10, capital_0=150000, capital_t=192013))

# (b)
t_1.b <- solve(method="compound", rate=sol_1.a, capital_0=150000, capital_t=250000)
(sol_1.b <- as.Date("2013-12-31") + years(ceiling(t_1.b)))


########################################################################
# Ex 2
(bc2.1 <- BaseContract(Dates=c("2020-12-31"),
                       CashFlows=c(12500)))

# (a)
(yc_flat1 <- MarketInterestRate(0.04, "2000-12-31"))
(sol_2.a <- value(bc2.1, by="2000-12-31", curve=yc_flat1, compound="compound"))
# check the result manually:
12500*(1+0.04)^-20

#### a few possible extensions now:
# linear rates
value(bc2.1, by="2000-12-31", curve=yc_flat1, compound="linear")
12500/(1+0.04*20)

# continuous rates
value(bc2.1, by="2000-12-31", curve=yc_flat1, compound="continuous")
12500*exp(-0.04*20)

# (b)
yc_flat1 <- MarketInterestRate(0.03, "2000-12-31")
yc_flat2 <- MarketInterestRate(0.05, "2010-12-31")
yc_flat3 <- MarketInterestRate(0.04, "2015-12-31")
(yc_dyn1 <- add(add(yc_flat1,yc_flat2),yc_flat3))

(sol_2.b <-value(bc2.1, by="2000-12-31", curve=yc_dyn1, compound="compound"))
12500*((1+0.04)^-5)*((1+0.05)^-5)*((1+0.03)^-10)

# (c)
(sol_2.c <- solve(method="compound", period=20, capital_0=sol_2.b[[1,1]], capital_t=12500))


########################################################################
# Ex 3
(bc3.1 <- BaseContract(Dates=c("2010-12-31"),
                       CashFlows=c(23000)))
(yc_flat <- MarketInterestRate(0.025, "2010-12-31"))
# It should be possible to choose the compounding period
(val <- value(bc3.1, by="2038-12-31", curve=yc_flat, compound="compound")) 

# via current "solve" function:
(t_3 <- solve(method="compound", rate=0.025, capital_0=23000, capital_t=46000))
(sol_1.b <- as.Date("2010-12-31") + years(ceiling(t_3)))

########################################################################
# Ex 4
(yc_flat <- MarketInterestRate(0.03, "2013-12-31"))
(yrly_dates <- as.character(seq(as.Date("2014-12-31"), as.Date("2018-12-31"), "years")))

(wlth <- wealth(yc_flat, 150000, yrly_dates, take_out=0.04, compound="compound", period="Y"))
# Here we need a recursive definition of the cash flow

########################################################################
# Ex 5
(bc5.1 <- BaseContract(Dates=c("2010-12-31"),
                       CashFlows=c(10000)))
yc_flat5.1 <- MarketInterestRate(0.02, "2010-12-31")
yc_flat5.2 <- MarketInterestRate(0.035, "2015-12-31")
(yc_dyn_5.1 <- add(yc_flat5.1,yc_flat5.2))

(val <- value(bc5.1, by="2020-12-31", curve=yc_dyn_5.1, compound="compound")) 
10000*(1+0.02)^5*(1+0.035)^5


########################################################################
# Ex 6
(bc6.1 <- BaseContract(Dates=c("2010-12-31"),
                       CashFlows=c(100000)))
(yc_flat <- MarketInterestRate(0.035, "2010-12-31"))
(val <- value(bc6.1, by="2025-12-31", curve=yc_flat, compound="compound")) 

(val <- value(bc6.1, by="2025-12-31", curve=yc_flat, compound="linear")) 
(val <- value(bc6.1, by="2025-12-31", curve=yc_flat, compound="compound"))
(val_1 <- value(bc6.1, by="2025-12-31", curve=yc_flat, compound="compound", period="Q"))
(val_2 <- value(bc6.1, by="2025-12-31", curve=yc_flat, compound="continuous"))
(sol_6.e1 <- solve(method="compound", period=15, capital_0=100000, capital_t=val_1[[1,1]]))


########################################################################
# Ex 7
(bc7.1 <- BaseContract(Dates=c("2029-01-01"), CashFlows=40000))
(yc_flat <- MarketInterestRate(0.025, "2020-01-01"))

f = function(invest) {
  value(bc7.1, by="2020-01-01", curve=yc_flat, compound="compound")[[1,1]] - invest
}
solve2(object=f, method="uniroot", lower=0, upper=40000)
# oder so (andere Methoden als compound müssen noch implementiert werden)
(sol_7 <- solve(method="compound", rate=0.025, period=9, capital_t=40000))


########################################################################
# Ex 8
(bc8.1 <- BaseContract(Dates=c("2020-01-01"), CashFlows=1000))
(yc_flat <- MarketInterestRate(0.032, "2020-01-01"))
f = function(tt) {
  by = as.Date("2020-01-01") + tt*365
  value(bc8.1, by=as.character(by), curve=yc_flat, compound="compound")[[1,1]] - 1500
}
solve2(object=f, method="uniroot", lower=1, upper=20)   ## Error!!

value(bc8.1, by=as.character(as.Date("2020-01-01")+12.8488*365), curve=yc_flat, compound="compound")
value(bc8.1, by="2032-11-02", curve=yc_flat, compound="compound")
value(bc8.1, by="2032-11-16", curve=yc_flat, compound="compound")

# oder so (andere Methoden als compound müssen noch implementiert werden)
(sol_8.a <- solve(method="compound", rate=0.032, capital_0=1000, capital_t=1500))



