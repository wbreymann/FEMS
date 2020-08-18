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

## create an instance of an Annuity algorithm
ann <- Ann()

## check what contract terms are available
terms(ann)

## link the contract to the risk factors
set(ann, rf)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what = list(DiscountingSpread = 0.0,
                     RiskFactorObject = yc))
set(eng, rf)
set(ann, eng)

## -----------------------------------------------------------------
## simple annuity (PAM with principal redemption cycle)
## -----------------------------------------------------------------
#' a simple Annuity:
#' Notional = 1000 CHF, Maturity after 2 years,
#' Interest and Redemption cycles in-line = 1 Year
set(ann, what = list(
         ContractID = "001",
         Currency = "CHF",
         Calendar = "MondayToFriday",
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31",     # on this day we analyse our PAM.
         ContractDealDate = "2012-12-31",
         InitialExchangeDate = "2013-01-01",  # here start the contract work
         MaturityDate = "2015-01-01",
         NotionalPrincipal = 1000,               # nominal value
         NominalInterestRate = 0.05,             # nominal Interest rate
         PremiumDiscountAtIED = 0.0,
         DayCountConvention = "30E/360",
         BusinessDayConvention = "SCF",
         CycleAnchorDateOfPrincipalRedemption = "2014-01-01",
         CycleOfPrincipalRedemption = "1Y-"))

#' generate contract events
as.data.frame(events(ann,ad))

#DONE UNTIL HERE.... Value doesnt work for annuity somehow...
#' compute nominal value
value(ann, by = ad, type = "nominal")
value(ann, by = c(ad, "2013-01-01", "2014-01-02"), type = "nominal")

#' compute mark-to-model value
value(ann,by=ad,type="markToModel")
value(ann,by=c(ad,"2013-01-02","2014-01-02"),type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# PR = Redemption, IP = Interest payment
# pay the notional of 1'000CHF
# get after one year: 150.00CHF (IP) + 465.12CHF (PR)
# get after two year: 80.23CHF (IP) + 534.89CHF (PR) (the second IP is
# smaller than the first because Notional amount becomes smaller
# than at the beginning)
# get in all: 150 + 465.12 + 80.23 + 534.89 = 1230.24CHF

## -----------------------------------------------------------------
## change the cycle of the redemption and cycle of interest payment
## and plot the events again
## -----------------------------------------------------------------
set(ann, what=list(CycleOfPrincipalRedemption = "6M-",
                   CycleAnchorDateOfPrincipalRedemption = "2013-07-01"))

#' generate contract events
as.data.frame(events(ann,ad))

#' compute mark-to-model value
value(ann,by="2013-01-02",type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# interest payment + redomption = const.
# pay 1000 CHF
# get 4 PR + 4 IP

## -----------------------------------------------------------------
## add one interest payment after 6M.... first redemption payment
## after 1 year  => grace period (=Gnadenfrist)
## -----------------------------------------------------------------
set(ann, what=list(CycleAnchorDateOfPrincipalRedemption = "2014-01-01",
                   CycleAnchorDateOfInterestPayment = "2013-07-01",
                   CycleOfInterestPayment = "6M-"))

#' generate contract events
as.data.frame(events(ann,ad))

#' compute mark-to-model value
value(ann,by="2013-01-02",type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# pay 1000 CHF
# get after 6M: Interest payment= 74.38
# get after 1year the first redemption + interest payment

## -----------------------------------------------------------------
## add capitalisation
## -----------------------------------------------------------------
set(ann, what=list(
         CapitalizationEndDate = "2013-07-01"))

#' generate contract events
as.data.frame(events(ann,ad))

#' compute mark-to-model value
value(ann,by="2013-01-02",type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# capitalize the first interest payment
# and get after 1year the redemption + interest payment

## -----------------------------------------------------------------
## add interest rate resetting after 1 year
## -----------------------------------------------------------------
set(object = ann, what = list(
                  CycleAnchorDateOfRateReset = "2014-01-01",
                  CycleOfRateReset = "6M-",
                  MarketObjectCodeRateReset = "YC_Prim",
                  FixingDays = "2D",
                  RateMultiplier = 1,
                  RateSpread = 0))

#' generate contract events
as.data.frame(events(ann,ad))

#' compute mark-to-model value
value(ann,by="2013-01-02",type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# use the last contract and reset the interest rate after 1year,
# the interest rate has changed (is lower than before)
# the green cycles in the lower part of the figure indicate that the interest
# rate is reset at 2014-01-01

## -----------------------------------------------------------------
## Amortization Date later than Maturity Date => ballooning
## -----------------------------------------------------------------
set(ann, what=list(
         AmortizationDate = "2020-01-01",
         MaturityDate= "2015-01-01" ))

#' generate contract events
as.data.frame(events(ann,ad))

#' compute mark-to-model value
value(ann,by="2013-01-02",type="markToModel")

#' plot contract events
plot(ann,ad)

# Summary of the plot:
# ~~~~~~~~~~~~~~~~~~~
# the annuity is not fully amortized, a balloon payment
# is required at the maturity date

