#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## -----------------------------------------------------------------
## import rActus library
## -----------------------------------------------------------------
rm(list = ls())
#library(rActus)

## ---------------------------------------------------------------
## Preparations
## ---------------------------------------------------------------
# specify analysis date
ad <- "2013-01-01T00"

## add yield curve and stock index to the market environment
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  Nodes = list(ReferenceDate = ad, Tenors = tenors, Rates = rates)))

ind <- Index()
times <- c("2011-12-31T00", "2012-12-31T00", "2013-12-31T00", "2014-12-31T00",
           "2015-12-31T00")
values <- c(100, 110, 120, 130, 140)
set(ind, what=list(
  MarketObjectCode = "CHF_SMI",
  Data=list(Dates=times,Values=values)))

# create actus risk factor connector (later linking of risk factor(s) and CT)
rf <- RFConn()
add(rf,list(yc,ind))
get(rf,"Keys")

## ---------------------------------------------------------------
## Future on Zero Coupon Bond without Margining
## ---------------------------------------------------------------

## define child
pam <- Pam()
set(pam, what=list(
  ContractID = "001_C1",
  StatusDate = "2012-12-31T00",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "RPA",                   # Real Position Asset
  ContractDealDate = "2012-12-31T00",
  InitialExchangeDate = "2014-01-03T00",  # here start the contract work
  MaturityDate = "2014-04-03T00",         # the day of the repayment
  NotionalPrincipal = 1000,               # nominal value
  PremiumDiscountAtIED = -100,
  DayCountConvention = "30E/360",
  BusinessDayConvention = "SCF"))

## set the valuation engine
eng <- DcEngine()
set(eng, what=list(dc.spread=0.0,
                   RiskFactorObjectLink="YC_Prim"))
set(eng, rf)
set(pam, eng)

## define parent
fut <- Futur()
set(fut, what=list(
  ContractID = "001",
  StatusDate = "2012-12-31T00",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "LG",
  ContractDealDate =  "2012-12-31T00",
  PurchaseDate = "2013-01-02T00",
  PriceAtPurchaseDate = 5.0,
  SettlementDate = "2014-01-01T00",
  FuturesPrice = 90))

## set the valuation engine
# we use the same (discounting) engine as for child
set(fut, eng)

## assign the child contract to parent
set(fut, pam)
get(fut, "ChildContracts")

#' generate contract events
as.data.frame(events(fut,ad))

#' compute nominal value
value(fut,by="2013-01-02",type="nominal")

#' compute markToModel value
value(fut,by="2013-01-02",type="markToModel")

#' plot contract events
plot(fut,ad)

## ---------------------------------------------------------------
## Future on Stock (without Margining)
## ---------------------------------------------------------------

## define the new child contract
stk <- Stk()
set(stk, what=list(
  ContractID = "1001",
  StatusDate = "2012-12-31T00",
  ContractRole = "RPA",
  Calendar = "Weekday",
  Currency = "CHF",
  ContractDealDate = "2012-12-31T00",
  PurchaseDate = "2014-01-04T00",
  PriceAtPurchaseDate = 90))

## set the child's valuation engine
eng <- CapmEngine()
set(eng, what=list(
  ModelAlpha=0.05,
  ModelBeta=0.8,
  ModelSigma=0.2,
  MarketValueObserved=100,
  RiskFreeRateTerm="10Y",
  IndexObjectLink="CHF_SMI",
  RiskFreeRatesObjectLink="YC_Prim",
  StatusDate="2012-12-31T00"))
set(eng, rf)
set(stk, eng)

## assign the child contract to parent
set(fut, stk)
get(fut, "ChildContracts")

#' generate contract events
as.data.frame(events(fut,ad))

#' compute markToModel value
value(fut,by="2013-01-02",type="markToModel")

#' plot contract events
plot(fut,ad)


## ---------------------------------------------------------------
## add Margining
## ---------------------------------------------------------------
set(fut, what=list(
  CycleAnchorDateOfMargining = "2013-01-02T00",
  CycleOfMargining = "1M-"))

#' generate contract events
as.data.frame(events(fut,ad))

#' compute markToModel value
value(fut,by="2013-01-02",type="markToModel")

#' plot contract events
plot(fut,ad)

## -----------------------------------------------------------------
## make Stock reference index scenario more interesting
## -----------------------------------------------------------------
times <- c("2011-12-31T00", "2012-12-31T00", "2013-02-01T00", 
           "2013-03-01T00", "2013-04-01T00", "2013-05-01T00",
           "2013-06-01T00", "2013-07-01T00", "2013-08-01T00",
           "2013-09-01T00", "2013-10-01T00", "2013-11-01T00",
           "2013-12-01T00", "2014-01-01T00")
values <- c(100, 110, 120, 150, 170, 180, 190, 195, 170,
            150, 120, 90, 95, 97)
set(ind, what=list(
  Data=list(Dates=times,Values=values)))

#' generate contract events
as.data.frame(events(fut,ad))

#' compute markToModel value
value(fut,by="2013-01-02",type="markToModel")

#' plot contract events
plot(fut,ad)

## -----------------------------------------------------------------
## add maintenance margin
## -----------------------------------------------------------------

set(fut, what=list(
  InitialMargin = 50,
  MaintenanceMarginLowerBound = 30,
  MaintenanceMarginUpperBound = 70))

#' generate contract events
as.data.frame(events(fut,ad))

#' compute markToModel value
value(fut,by="2013-01-02",type="markToModel")

#' plot contract events
plot(fut,ad)

