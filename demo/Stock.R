#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## -----------------------------------------------------------------
## import rActus library
## -----------------------------------------------------------------
rm(list = ls())
library(rActus)

## ---------------------------------------------------------------
## Preparations
## ---------------------------------------------------------------
# specify analysis date
ad <- "2012-12-31"

## add yield curve and stock index to the market environment
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  ReferenceDate = ad, 
  Tenors = tenors, 
  Rates = rates))

ind <- Index()
times <- c("2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31",
           "2015-12-31")
values <- c(100, 110, 120, 130, 140)
set(ind, what=list(
  MarketObjectCode = "CHF_SMI",
  Data = list(Dates=times, Values=values)))

div <- Index()
times <- c("2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31",
           "2015-12-31")
values <- c(5, 4, 8, 0, 5)
set(div, what=list(
  MarketObjectCode = "DIV_Prim",
  Data=list(Dates=times,Values=values)))

# create actus risk factor connector (later linking of risk factor(s) and CT)
rf <- RFConn()
add(rf,list(yc,ind, div))
get(rf,"Keys")

## new Stock algorithm instance
stk <- Stk()

## check what contract terms are available
terms(stk)

## link the contract to the risk factors
set(stk, rf)

## create valuation engine, link to risk factors and assign to stk
eng <- CapmEngine()
set(eng, what=list(
  ModelAlpha=0.05,
  ModelBeta=0.8,
  ModelSigma=0.2,
  MarketValueObserved=100,
  RiskFreeRateTerm="10Y",
  IndexObjectLink="CHF_SMI",
  RiskFreeRatesObjectLink="YC_Prim",
  StatusDate="2012-12-29"))
set(eng, rf)
set(stk, eng)

## -----------------------------------------------------------------
## 1. Stock without dividend payment
## -----------------------------------------------------------------
#' define a stock contract without any dividend payment
#' the market value is 90,- at begin
#' the stock has no termination date
set(stk, what=list(
  StatusDate = "2012-01-01",
  ContractID = "1001",
  ContractRole = "RPL",
  Currency = "CHF",
  ContractDealDate = "2012-12-31",
  PurchaseDate = "2013-01-02",
  PriceAtPurchaseDate = 90))

#' generate contract events
as.data.frame(events(stk,ad))

#' compute nominal value
value(stk,by="2013-01-02",type="nominal")

#' compute markToModel value
value(stk,by="2013-01-02",type="markToModel")

#' plot contract events
plot(stk,ad)

# summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# At the 01.01.2013 you have to pay the stockvalue of 90,- (first red Arrow)
# The upwards direction of the first arrow shows you that this is a asset position.


## -----------------------------------------------------------------
## 2. with Termination Date
## -----------------------------------------------------------------
#' same ContractTerms as 1 but contract have a termination date at 2015-01-31
set(stk, what=list(TerminationDate = "2015-01-31",
                   PriceAtTerminationDate = 100))

#' generate contract events
as.data.frame(events(stk,ad))

#' compute markToModel value
value(stk,by="2013-01-02",type="markToModel")
  
#' plot contract events
plot(stk,ad)

# summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# At the 01.01.2013 you have to pay the stockvalue of 90,- (first red Arrow)
# At the 31.12.2015 you get repaid the notional of 100,- (last red Arrow)


## -----------------------------------------------------------------
## 3. Stock with dividend payment
## -----------------------------------------------------------------
#' same ContractTerms as 2
#' steady dividend rate with 0.05
set(stk, what=list(
  CycleAnchorDateOfDividend = "2013-01-01",
  CycleOfDividend           = "6M-",
  MarketObjectCodeOfDividendRate = "DIV_Prim"))

#' generate contract events
as.data.frame(events(stk,ad))

#' compute markToModel value
value(stk,by="2013-01-02",type="markToModel")

#' plot contract events
plot(stk,ad)

# summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# The blue arrows indicate the cyclic dividend payments at changing 
# rate according to the DIV_Prim risk factor


## -----------------------------------------------------------------
## 4. example 3 but as Liability
## -----------------------------------------------------------------
#' same ContractTerms as 3 but contract role is liability
set(stk, what=list(ContractRole = "RPL"))     # Real Position Liability

#' generate contract events
as.data.frame(events(stk,ad))

#' compute markToModel value
value(stk,by="2013-01-02",type="markToModel")

#' plot contract events
plot(stk,ad)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# The opposite directions of the arrows shows you that this is a liability position.

