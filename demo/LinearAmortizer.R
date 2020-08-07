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

## create an instance of a Principal At Maturity algorithm
lam <- Lam()

## check what contract terms are available
terms(lam)

## link the contract to the risk factors
set(lam, rf)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what = list(DiscountingSpread = 0.0,
                     RiskFactorObject = yc))
set(eng, rf)
set(lam, eng)

## -----------------------------------------------------------------
## negative amortizer
## -----------------------------------------------------------------
## Notional = 1000 CHF, Cycle Of Interest Payment = 1 Year,
## Maturity Date = 2016-01-01, Cycle Of Principal Redemption = 1 Year
## Cycle of Rate Reset = 2 Years, Next Principal Redemption Payment = 200
set(lam, what=list(
         ContractID = "001",
         Currency = "CHF",
         Calendar = "MondayToFriday",
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31",     # on this day we analyse our PAM.
         ContractDealDate = "2012-12-31",
         InitialExchangeDate = "2013-01-02",  # here start the contract work
         MaturityDate = "2016-01-02",
         NotionalPrincipal = 1000,               # nominal value
         NominalInterestRate = 0.05,             # nominal Interest rate
         PremiumDiscountAtIED = 0.0,
         DayCountConvention = "30E/360",
         BusinessDayConvention = "SCF",
         CycleAnchorDateOfPrincipalRedemption = "2014-01-02",
         CycleOfPrincipalRedemption = "1Y-",
         CycleAnchorDateOfInterestPayment = "2014-01-02",
         CycleOfInterestPayment = "1Y-",
         NextPrincipalRedemptionPayment = 200))

#' generate contract events
as.data.frame(events(lam,ad))

#' compute nominal value
value(lam,by="2013-01-02",type="nominal")

#' compute markToModel value
value(lam,by="2013-01-02",type="markToModel",method=eng)

#' plot contract events
plot(lam,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# ytc

## -----------------------------------------------------------------
## change the cycle of the redemption and cycle of interest payment
## and plot the events again
## -----------------------------------------------------------------
set(lam, what=list(CycleOfPrincipalRedemption = "6M-",
                   CycleAnchorDateOfPrincipalRedemption = "2013-07-02"))

#' generate contract events
as.data.frame(events(lam,ad))

#' compute nominal value
value(lam,by="2013-01-02",type="nominal")

#' compute markToModel value
value(lam,by="2013-01-02",type="markToModel")

#' plot contract events
plot(lam,ad)

