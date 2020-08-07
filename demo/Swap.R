#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## ---------------------------------------------------------------
## import rAcctus library
## ---------------------------------------------------------------
rm(list = ls())
library(rActus)

## ---------------------------------------------------------------
## Preparations
## ---------------------------------------------------------------
# specify analysis date
ad <- "2012-12-31T00"

# create yield curve (for rate reset and valuation)
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  Nodes = list(ReferenceDate = ad, Tenors = tenors, Rates = rates)))

# create actus risk factor connector (later linking of risk factor(s) and CT)
rf <- RFConn()
add(rf,yc)
get(rf,"Keys")

## ---------------------------------------------------------------
## Plain Vanilla Swap
## ---------------------------------------------------------------
## first leg - a fixed PAM
leg1 <- Pam()
set(leg1, what=list(
  ContractID = "001",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "RPA",                   # Real Position Asset
  StatusDate       = "2012-12-31T00",     # per this date terms are valid
  ContractDealDate = "2012-12-31T00",
  InitialExchangeDate = "2013-01-01T00",  # here start the contract work
  MaturityDate = "2016-01-01T00",         # the day of the repayment
  NotionalPrincipal = 1000,               # nominal value
  NominalInterestRate = 0.05,             # nominal Interest rate
  DayCountConvention = "30E/360",
  BusinessDayConvention = "SCF", 
  CycleAnchorDateOfInterestPayment = "2014-01-01T00",
  CycleOfInterestPayment = "1Y-"))

## link the contract to the risk factors
set(leg1, rf) # not really necessary here since no RiskFactor-linked events

## second leg - a floating PAM
leg2 <- Pam()
set(leg2, what=list(
  ContractID = "001",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "RPL",                   # Real Position Asset
  StatusDate       = "2012-12-31T00",     # per this date terms are valid
  ContractDealDate = "2012-12-31T00",
  InitialExchangeDate = "2013-01-01T00",  # here start the contract work
  MaturityDate = "2016-01-01T00",         # the day of the repayment
  NotionalPrincipal = 1000,               # nominal value
  NominalInterestRate = 0.05,             # nominal Interest rate
  DayCountConvention = "30E/360",
  BusinessDayConvention = "SCF",
  CycleAnchorDateOfInterestPayment = "2014-01-01T00",
  CycleOfInterestPayment = "1Y-",
  CycleAnchorDateOfRateReset = "2014-01-01T00",
  CycleOfRateReset = "1Y-",
  MarketObjectCodeRateReset="YC_Prim"))

## link the contract to the risk factors
set(leg2, rf)

## ---------------------------------------------------------------
## Define parent contract - the Swap
## ---------------------------------------------------------------
swap <- Swaps()
set(swap, what=list(
  ContractID = "001",
  Currency = "CHF",
  ContractRole = "RFL", # receive fixed leg
  StatusDate       = "2012-12-31T00",
  ContractDealDate = "2012-12-31T00",
  DeliverySettlement = "D"))

## assign the two legs
set(swap, leg1, leg2)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what=list(DiscountingSpread=0.1,
                   RiskFactorObjectLink="YC_Prim"))
set(eng, rf)
set(swap, eng)

#' generate contract events
as.data.frame(events(swap,ad))

#' compute nominal value
value(swap,by="2013-01-02",type="nominal")

#' compute markToModel value
value(swap,by="2013-01-02",type="markToModel")

#' plot contract events
plot(swap,ad)

## ---------------------------------------------------------------
## Roller Coaster Swap
## ---------------------------------------------------------------
## first leg - a fixed LAX
leg1 <- Lax()
set(leg1, what=list(
  ContractID = "001",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "RPA",                   # Real Position Asset
  StatusDate       = "2012-12-31T00",     # on this day we analyse our PAM.
  ContractDealDate = "2012-12-31T00",
  InitialExchangeDate = "2013-01-02T00",  # here start the contract work
  NotionalPrincipal = 1000,               # nominal value
  NominalInterestRate = 0.05,             # nominal Interest rate
  DayCountConvention = "30E/360",
  ArrayCycleAnchorDateOfPrincipalRedemption = "2013-07-01T00, 2015-07-01T00",
  ArrayCycleOfPrincipalRedemption = "6M-, 1Y-",
  ArrayNextPrincipalRedemptionPayment = "100, 500",
  ArrayIncreaseDecrease = "INC, DEC"))

## link the contract to the risk factors
set(leg1, rf) # not really necessary here since no RiskFactor-linked events

## second leg - a floating LAX
leg2 <- Lax()
set(leg2, what=list(
  ContractID = "001",
  Currency = "CHF",
  Calendar = "Weekday",
  ContractRole = "RPA",                   # Real Position Asset
  StatusDate       = "2012-12-31T00",     # on this day we analyse our PAM.
  ContractDealDate = "2012-12-31T00",
  InitialExchangeDate = "2013-01-02T00",  # here start the contract work
  NotionalPrincipal = 1000,               # nominal value
  NominalInterestRate = 0.05,             # nominal Interest rate
  DayCountConvention = "30E/360",
  ArrayCycleAnchorDateOfPrincipalRedemption = "2013-07-01T00, 2015-07-01T00",
  ArrayCycleOfPrincipalRedemption = "6M-, 1Y-",
  ArrayNextPrincipalRedemptionPayment = "100, 500",
  ArrayIncreaseDecrease = "INC, DEC",
  ArrayCycleAnchorDateOfRateReset = "2014-07-02T00, 2015-07-02T00, 2016-07-02T00",
  ArrayCycleOfRateReset = "NULL, NULL, NULL",
  ArrayFixedVariable = "FIX, FIX, FIX",
  ArrayRate = "0.02, 0.03, 0.05",
  MarketObjectCodeRateReset = "YC_Prim"))

## link the contract to the risk factors
set(leg2, rf)

## ---------------------------------------------------------------
## Define parent contract - the Swap
## ---------------------------------------------------------------
swap <- Swaps()
set(swap, what=list(
  ContractID = "001",
  Currency = "CHF",
  ContractRole = "RFL", # receive fixed leg
  StatusDate       = "2012-12-31T00",
  ContractDealDate = "2012-12-31T00",
  DeliverySettlement = "D"))

## assign the two legs
set(swap, leg1, leg2)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what=list(DiscountingSpread=0.0,
                   RiskFactorObjectLink="YC_Prim"))
set(eng, rf)
set(swap, eng)

#' generate contract events
as.data.frame(events(swap,ad))

#' compute nominal value
value(swap,by="2013-01-02",type="nominal")

#' compute markToModel value
value(swap,by="2013-01-02",type="markToModel")

#' plot contract events
plot(swap,ad)
