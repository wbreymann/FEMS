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

## create yield curves per currency and an FX-rate risk factors
yc1 <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc1, what = list(Nodes = list(
  ReferenceDate = ad,
  Rates = rates, Tenors = tenors),
  MarketObjectCode = "CHF_Prim"))

yc2 <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.003, 0.005, 0.01, 0.02, 0.03, 0.04)
set(yc2, what = list(Nodes = list(
  ReferenceDate = ad,
  Rates = rates, Tenors = tenors),
  MarketObjectCode = "USD_Prim"))

fx <- FxRate()
times <- c("2011-12-31T00", "2012-12-31T00", "2013-12-31T00", 
           "2014-12-31T00", "2015-12-31T00")
values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
set(fx, what=list(
  MarketObjectCode = "CHF/USD",
  CurrencyPair = "CHF/USD",
  Data=list(Dates=times,Values=values)))

rf <- RFConn()
add(rf, list(yc1, yc2, fx))
get(rf, "Keys")

## new ForeignExchangeOutright algorithm instance
fxout <- Fxout()

## link risk factors to the contract
set(fxout, rf)

## create a Cross-Currency Discounting Engine for valuation
eng <- XfxDcEngine()
set(eng, 
    what=list(CurrencyPair="CHF/USD",
              TargetCurrency="CHF",
              InterestRateModelObjectLinks=list(CHF="CHF_Prim",
                                                USD="USD_Prim"),
              SpreadCollection=list(CHF=0.01,
                                    USD=0.02)))

## link valuation engine to risk factors and assign to contract
set(eng, rf)
set(fxout, eng)

## -----------------------------------------------------------------
## A Foreign Exchange Outright contract where currencies are delivered
## -----------------------------------------------------------------
set(fxout, what=list(
  ContractID = "1001",
  ContractRole = "RFL",
  StatusDate = "2012-12-31T00",
  ContractDealDate = "2012-12-31T00",
  MaturityDate = "2015-01-01T00",
  Currency = "CHF",
  Currency2="USD",
  NotionalPrincipal=1000,
  NotionalPrincipal2=1100))

#' generate contract events
as.data.frame(events(fxout,ad))

#' compute nominal value
value(fxout,by="2013-01-02",type="nominal")

## -----------------------------------------------------------------
## same but with settlement of currencies
## -----------------------------------------------------------------
set(fxout, what=list(
  SettlementDate = "2014-12-29T00"))

#' generate contract events
as.data.frame(events(fxout,ad))

#' compute nominal value
value(fxout,by="2013-01-02",type="nominal")
