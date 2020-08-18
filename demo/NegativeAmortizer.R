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

## create an instance of a Principal At Maturity algorithm
nam <- Nam()

## check what contract terms are available
terms(nam)

## link the contract to the risk factors
set(nam, rf)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what=list(DiscountingSpread=0.0,
                   RiskFactorObjectLink="YC_Prim"))
set(eng, rf)
set(nam, eng)


## -----------------------------------------------------------------
## negative amortizer
## -----------------------------------------------------------------
## Notional = 1000 CHF, Cycle Of Interest Payment = 1 Year,
## Maturity Date = 2016-01-01T00, Cycle Of Principal Redemption = 1 Year
## Cycle of Rate Reset = 2 Years, Next Principal Redemption Payment = 200
set(nam, what=list(
         ContractID = "001",
         Currency = "CHF",
         Calendar = "Weekday",
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31T00",     # on this day we analyse our PAM.
         ContractDealDate = "2012-12-31T00",
         InitialExchangeDate = "2013-01-02T00",  # here start the contract work
         MaturityDate = "2016-01-02T00",
         NotionalPrincipal = 1000,               # nominal value
         NominalInterestRate = 0.05,             # nominal Interest rate
         PremiumDiscountAtIED = 0.0,
         DayCountConvention = "30E/360",
         BusinessDayConvention = "SCF",
         CycleAnchorDateOfPrincipalRedemption = "2014-01-02T00",
         CycleOfPrincipalRedemption = "1Y-",
         NextPrincipalRedemptionPayment = 200,
         CycleAnchorDateOfRateReset = "2015-01-02T00",
         CycleOfRateReset = "2Y-",
         MarketObjectCodeRateReset = "YC_Prim"))

#' generate contract events
as.data.frame(events(nam,ad))

#' compute nominal value
value(nam,by="2013-01-02",type="nominal")

#' compute markToModel value
value(nam,by="2013-01-02",type="markToModel")

#' plot contract events
plot(nam,ad)

## -----------------------------------------------------------------
## change the cycle of the redemption and cycle of interest payment
## and plot the events again
## -----------------------------------------------------------------
set(nam, what=list(CycleOfPrincipalRedemption = "6M-",
                   CycleAnchorDateOfPrincipalRedemption = "2013-07-02T00"))

#' generate contract events
as.data.frame(events(nam,ad))

#' compute nominal value
value(nam,by="2013-01-02",type="nominal")

#' compute markToModel value
value(nam,by="2013-01-02",type="markToModel")

#' plot contract events
plot(nam,ad)

