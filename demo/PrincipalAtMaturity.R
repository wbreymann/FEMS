#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## ---------------------------------------------------------------
## import rActus library
## ---------------------------------------------------------------
rm(list = ls())
library(FEMS)

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
containsID(rf, "YC_Prim")

## create an instance of a Principal At Maturity algorithm
pam <- Pam()

## check what contract terms are available
terms(pam)

## link the contract to the risk factors
set(pam, rf)

## create valuation engine, link to risk factors and assign to pam
eng <- DcEngine()
set(eng, what = list(DiscountingSpread = 0.0,
         RiskFactorObject = yc))
set(eng, rf)
set(pam, eng)

## ---------------------------------------------------------------
## 1. Zero Coupon Bond
## ---------------------------------------------------------------
#' define a Zero Coupon contract
#' only one interest payment at maturity date
#' steady interest rate with 0.00
#' contract term is 3 months
#' specify the contract terms
set(pam, what=list(
         ContractID = "001",
         Currency = "CHF",
         Calendar = "MF",
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31",     # on this day we analyse our PAM.
         ContractDealDate = "2012-12-31",
         InitialExchangeDate = "2013-01-01",  # here start the contract work
         MaturityDate = "2013-03-31",         # the day of the repayment
         NotionalPrincipal = 1000,               # nominal value
         NominalInterestRate = 0.00,             # nominal Interest rate
         PremiumDiscountAtIED = -100,
         DayCountConvention = "30E360",
         BusinessDayConvention = "SCF"))
#' get values of specific ContractTerms
get(pam, what = "ContractID")
#' show summary of important ContractTerms
summary(pam)

#' generate contract events
as.data.frame(events(pam,ad))

#' compute nominal value
value(pam,by = ad,type = "nominal")
value(pam,by = c(ad,"2013-01-01","2014-01-02"), type = "nominal")

#' compute mark-to-model value
value(pam, by = ad,type = "markToModel", method = eng)
value(pam, by = c(ad,"2013-01-02","2014-01-02"), type = "markToModel", method = eng)

#' compute marginal liquidity
by <- timeSequence(substring(ad,1,10), by = "1 week", length.out = 15)
liquidity(pam, by = by, type = "marginal")

#' compute cumulative liquidity
liquidity(pam, by = by, type = "cumulative")

# nominal income vector
income(pam, by = by, type = "marginal", revaluation.gains = FALSE)

# book-income vectorS
income(pam, by = by, type="marginal", revaluation.gains = TRUE, method = eng)

#' plot contract events
plot(pam,ad)

#' compute sensitivity
sensitivity(pam,by = ad,type="macaulay",method = eng)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# At the 01.01.2013 you have to pay the notional of 1000,-
# minus premium discount of 100,- (first red Arrow)
# The premium discount compensates for the zero interest rates.
# The upwards direction of the first arrow shows you that this is an asset position
# At the 31.03.2013 you get repaid the notional of 1000,- (last red Arrow)

#' reset certain ContractTerms
set(pam, what = list(PremiumDiscountAtIED = 0))

## ---------------------------------------------------------------
## 2. standard PAM with single coupon at maturity
## ---------------------------------------------------------------
#' define a PAM contract with 1000 nominal value
#' only one interest payment at maturity date
#' steady interest rate with 0.05 and contract term with 2 years
set(pam, what = list(
         ContractRole = "RPA",                   # Real Position Asset
         StatusDate       = "2012-12-31",     # on this day we analyse our PAM.
         InitialExchangeDate = "2013-01-01",  # here start the contract work
         MaturityDate = "2014-12-31",         # the day of the repayment
         NominalInterestRate = 0.05    ))        # nominal Interest rate

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' plot contract events
plot(pam,ad)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# At the 01.01.2013 you have to pay the notional of 1000,- (first red Arrow)
# The upwards direction of the first arrow shows you that this is a asset position.
# the interest paid at maturity is 149.86

## ---------------------------------------------------------------
## 3. example 2 but as Liability
## ---------------------------------------------------------------
#' same ContractTerms as 2 but contract role is liability
set(pam, what=list(
         ContractRole = "RPL",                    # Real Position Liability
         NominalInterestRate = 0.05))             # nominal Interest rate

#' generate contract events
as.data.frame(events(pam,ad)) # here is no interest payment without cycle definition...

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' plot contract events
plot(pam,ad)

#' Summery of the Plot
# ~~~~~~~~~~~~~~~~~~~
#' The opposite directions of the arrows shows you that this is a liability position.
set(pam, what = list(ContractRole = "RPA"))

## ---------------------------------------------------------------
## 4. add cyclic coupon payments
## ---------------------------------------------------------------
#' define a PAM contract with 6 months coupon payment cycle
#' steady interest rate of 5% p.a. and contract term with 2 years
set(pam, what = list(
         CycleAnchorDateOfInterestPayment = "2013-01-01",  # start of the coupon payment
         CycleOfInterestPayment = "P6ML0"))                      # was "6M-" not sure what the L0 does?

#' generate contract events
as.data.frame(events(pam,ad)) 

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' plot contract events
plot(pam,ad)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# The direction of the first row show you that this is an asset
# The green arrows indicate the cyclic coupon payments of about 25
# coupon payments vary according to the day count between payments
# notice the significantly smaller coupon payment at MD due to the
# ...shorter last interest period (by 1 month)

## ---------------------------------------------------------------
## 5. An another way to handle the last (short) coupon payment
## ---------------------------------------------------------------
set(pam, what = list(CycleOfInterestPayment = "P6ML1")) # was "6M+" not sure what the L1 does?

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' plot contract events
plot(pam,ad)

# Summary of the Plot
# the coupon payment leading to a short last coupon period has been
# ... suppressed. Thus, a long last coupon period results.
set(pam, what = list(CycleOfInterestPayment = "P6ML0")) # was "6M-" not sure what the L0 does?

## ---------------------------------------------------------------
## 6. add capitalisation of interest payments
## ---------------------------------------------------------------
#' now we use the last contract and add a capitalisation
#' until the first Period of 6 month.
set(object = pam, what = list(CapitalizationEndDate = "2013-07-01" ))

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' Capitalized IP don't show up in liquidity
by=c("2012-12-31","2013-06-30", "2013-12-31", "2014-03-30", "2014-12-31")
tb = timeBuckets(by, c("2013.H1", "2013.H2", "2014.H1", "2014.H2"))
liquidity(pam, by=tb, type="marginal")

#' plot contract events
plot(pam,ad)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# Until the capitalization end date interest payments are added to the
#  ... outstanding notional
# notice the increase at 2013-07-31 of the outstanding notional indicated
#  ...  by the red dotted line
# The first real coupon payment happens at the 31.01.2014. (first green Arrow)

## ---------------------------------------------------------------
## 7. floating rate instrument
## ---------------------------------------------------------------
#' we use the last contract and reset the rates yearly
set(object = pam, what = list(
                  CycleAnchorDateOfRateReset = "2014-01-01",
                  CycleOfRateReset = "P1YL0", # was "1Y-"
                  MarketObjectCodeOfRateReset = "YC_Prim"))

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel", method=eng)

#' plot contract events
plot(pam,ad)

# Summary of the Plot
# ~~~~~~~~~~~~~~~~~~~
# The green cycles in the lower part of the figure indicate that the interest
# ... rate is reset at 2014-01-31. At the RR event a new rate is picked up
# ... in the market (yield curve) which is used for the interest payments
# ... from thereon.


## ---------------------------------------------------------------
## 8. rate reset ContractTerms
## ---------------------------------------------------------------
#' we also use the last contract but reset the interest rate bi-annually
set(object = pam, what = list(
                  CycleAnchorDateOfRateReset = "2013-07-01",
                  CycleOfRateReset = "P3ML0")) # was "3M-"

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel",method=eng)

#' plot contract events
plot(pam,ad)

## ---------------------------------------------------------------
## 9. change yield curve 
## ---------------------------------------------------------------
#' we use the same contract but add a spread of 10% to the 
#' yield curve used for rate resetting and discounting
tenors <- get(yc,"Tenors")
rates <- get(yc,"Rates")+0.1

set(yc, what = list(
          ReferenceDate = ad,
          Rates=rates,
          Tenors=tenors))

#' generate contract events
as.data.frame(events(pam,ad))

#' compute mark-to-model value
value(pam,by="2013-01-02",type="markToModel",method=eng)

#' plot contract events
plot(pam,ad)

# Summery of the Plot
# ~~~~~~~~~~~~~~~~~~~
# Now you see the PAM with the new interest rates

