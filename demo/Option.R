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
## European Call Option on Zero Coupon Bond
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
opt <- Optns()
set(opt, what=list(
  ContractID = "001",
  StatusDate = "2012-12-31T00",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "Buyer",
  ContractDealDate =  "2012-12-31T00",
  PurchaseDate = "2013-01-02T00",
  PriceAtPurchaseDate = 5.0,
  OptionType = "C",          # C=Call
  OptionExecutionType = "E", # E=European
  DeliverySettlement = "S",  # S=Settlement
  OptionStrike1 = 90,        # percentage of notional for FI-underlyings
  OptionExerciseEndDate = "2014-01-01T00"))

## set the valuation engine
# we use the same (discounting) engine as for child
set(opt, eng)

## assign the child contract to parent
set(opt, pam)
get(opt, "ChildContracts")

#' generate contract events
as.data.frame(events(opt,ad))

#' compute nominal value
value(opt,by="2013-01-02",type="nominal")

#' compute markToModel value
value(opt,by="2013-01-02",type="markToModel")

#' plot contract events
plot(opt,ad)

## -----------------------------------------------------------------
## Same as before but Put Option
## -----------------------------------------------------------------
set(opt, what=list(OptionType="P"))

#' generate contract events
as.data.frame(events(opt,ad))

#' compute markToModel value
value(opt,by="2013-01-02",type="markToModel")

#' plot contract events
plot(opt,ad)

## -----------------------------------------------------------------
## Call Option as before but now Option Seller
## -----------------------------------------------------------------
set(opt, what=list(
  OptionType="C",
  ContractRole="Seller"))

#' generate contract events
as.data.frame(events(opt,ad))

#' compute markToModel value
value(opt,by="2013-01-02",type="markToModel")

#' plot contract events
plot(opt,ad)

## -----------------------------------------------------------------
## Change Strike Price so option ends "out-of-the-money"
## -----------------------------------------------------------------
set(opt, what=list(OptionStrike1=110.0))

#' generate contract events
as.data.frame(events(opt,ad))

#' compute markToModel value
value(opt,by="2013-01-02",type="markToModel")

#' plot contract events
plot(opt,ad)

## ---------------------------------------------------------------
## Same Option but now on Stock underlying
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
set(opt, stk)
get(opt, "ChildContracts")

#' generate contract events
as.data.frame(events(opt,ad))

#' compute markToModel value
value(opt,by="2013-01-02",type="markToModel")


#' plot contract events
plot(opt,ad)
