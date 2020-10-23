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
ad <- "2012-12-31"

# create yield curve (for rate reset and valuation)
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  ReferenceDate = ad, 
  Tenors = tenors, 
  Rates = rates))
setTimeSeries(yc, yc$ReferenceDate, as.character(today()))

# create actus risk factor connector (later linking of risk factor(s) and CT)
rf <- RFConn()
add(rf,yc)
get(rf,"Keys")

## create an instance of an Exotic Linear Amortizer algorithm
lax <- Lax()

## check what contract terms are available
terms(lax)

## link the contract to the risk factors
set(lax, rf)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
# set(eng, what=list(dc.spread=0.0,
#                    RiskFactorObjectLink="YC_Prim"))
set(eng, what = list(dc.spread = 0.0,
                     dc.object = yc))
set(eng, rf)
set(lax, eng)

## -----------------------------------------------------------------
## exotic linear amortizer
## -----------------------------------------------------------------
## InitialExchangeDate (start of the contract) = "2013-01-02"
## Notional = 1000 CHF
## Two Principal Redemption periods:
## 1: step-up phase (increasing nominal amount): 
##    - Anchor date of this phase: "2013-07-01"
##    - Cycle Of Principal Redemption = 6 Months
##    - Amount to increase = 100
## 2: draw-down phase (decreasing nominal amount):
##    - Anchor date of this phase: "2015-07-01"
##    - Cycle Of Principal Redemption = 1 Year
##    - Amount to decrease = 500

set(lax, what = list(
         ContractID = "001",
         Currency = "CHF",
         Calendar = "MondayToFriday",
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31",     # on this day we analyse our PAM.
         ContractDealDate = "2012-12-31",
         InitialExchangeDate = "2013-01-02",  # here start the contract work
         NotionalPrincipal = 1000,               # nominal value
         NominalInterestRate = 0.05,             # nominal Interest rate
         DayCountConvention = "30E/360",
         ArrayCycleAnchorDateOfPrincipalRedemption = "2013-07-01, 2015-07-01",
         ArrayCycleOfPrincipalRedemption = "6M-, 1Y-",
         ArrayNextPrincipalRedemptionPayment = "100, 500",
         ArrayIncreaseDecrease = "INC, DEC"))

#' generate contract events
as.data.frame(events(lax,ad))

#' compute mark-to-model value
value(lax,by="2013-01-02",type="markToModel")

#' plot contract events
plot(lax,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# ytc

## -----------------------------------------------------------------
## add fixed rate reset schedule that changes into variable pattern
## 1. change fixed rate to 0.02 on 2014-07-02
## 2. change fixed rate to 0.03 on 2015-07-02
## 3. change to variable rate with 0.05 spread on 2016-07-02
## -----------------------------------------------------------------
set(lax, what=list(ArrayCycleAnchorDateOfRateReset = "2014-07-02, 2015-07-02, 2016-07-02",
                   ArrayCycleOfRateReset = "NULL, NULL, 6M-",
                   ArrayFixedVariable = "FIX, FIX, VAR",
                   ArrayRate = "0.02, 0.03, 0.005",
                   MarketObjectCodeRateReset = "YC_Prim"))

#' generate contract events
as.data.frame(events(lax,ad))

#' compute mark-to-model value
value(lax,by="2013-01-02",type="markToModel")

#' plot contract events
plot(lax,ad)

