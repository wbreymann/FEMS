#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

rm(list = ls())

## -----------------------------------------------------------------
## import libraries
## -----------------------------------------------------------------
#library(rActus)

## set analysis date
ad <- "2015-01-02"

## -----------------------------------------------------------------
## import a portfolio of contracts
## -----------------------------------------------------------------

## new Portfolio instance
ptf <- Portfolio()
import(ptf, source = "./data/BondPortfolio.xls", sheet="BondPortfolio", valuationEngines=TRUE)

# ## via java Excel loader, ...
# import(ptf,
#        source="./data/BondPortfolio.xls",
#        sheet="BondPortfolio", valuationEngines=TRUE)
# 
# ## or via java flat file loader
# import(ptf,
#        source="./data/BondPortfolio.csv",
#        sep=",", valuationEngines=TRUE)

## show portfolio composition
ptf

## show contract ids
get(ptf, "ids")

## -----------------------------------------------------------------
## initialize market environment
## -----------------------------------------------------------------

## market yield curve
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_EA_AAA",
  ReferenceDate = ad, 
  Tenors = tenors, 
  Rates = rates))
setTimeSeries(yc, yc$ReferenceDate, as.character(today()))

## market consumer price index
cpi <- Index()
times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
           "2019-01-01")
values <- c(100, 110, 120, 130, 140)
set(cpi, what=list(
  MarketObjectCode = "IND_CPI_EA",
  Data=list(Dates=times,Values=values)))

# create actus risk factor connector
rf <- RFConn()
add(rf, list(yc, cpi))
get(rf, "keys")

## ---------------------------------------------------------------
## link contracts to the (market) risk factor
## ---------------------------------------------------------------
set(ptf, rf)

## compute events for single contract
as.data.frame(events(get(ptf, "105"),ad))
as.data.frame(events(get(ptf, "112"),ad))
## compute events for entire portfolio
as.data.frame(events(ptf,ad))

# compute analytics

# for single time
value(ptf,ad,type="nominal")

# for multiple times
by=timeSequence(substring(ad,1,10),"2020-06-01",by="1 year")
value(ptf,by,type="nominal")

## compute mark-to-model value
## -> define discounting model
dcEngine <- DcEngine()
set(dcEngine,list(RiskFactorObjectLink="YC_EA_AAA",
                  DiscountingSpread=0.0))
set(dcEngine,rf)
value(ptf,by,type="markToModel",method=dcEngine)

# marginal liquidity-vector
liquidity(ptf,by=by,type="marginal")

# cumulative liquidity-vector
liquidity(ptf,by=by,type="cumulative")

# nominal income vector
income(ptf,by=by,type="marginal",revaluation.gains=FALSE)

# book-income vector
income(ptf,by=by,type="marginal",revaluation.gains=TRUE)

## compute sensitivity of portfolio-value w.r.t. shift in risk factors
## -> define shift scenario
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_EA_AAA",
  Nodes = list(ReferenceDate = ad, 
               Tenors = tenors, Rates = rates+0.2)))
cpi <- Index()
times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
           "2019-01-01")
values <- c(100, 110, 120, 130, 140)
set(cpi, what=list(
  MarketObjectCode = "IND_CPI_EA",
  Data=list(Dates=times,Values=values)))
rf.shift <- RFConn()
add(rf.shift, list(yc, cpi))

# compute sensitivity
sensitivity(ptf,ad,type="numeric",method=dcEngine,scenarios=c(rf,rf.shift))

## add portfolio structure
# balance sheet
tree = Tree(list(
  branches = list( 
    Top = c("Portfolio1","Portfolio2"),
    Portfolio1 = c("Sub11","Sub12"),
    Portfolio2 = c("Sub21","Sub22")
  ),
  leafs=list(
    Sub11 = c("101","102","103"), 
    Sub12 = "104",
    Sub21 = "105",
    Sub22 = "106"
  )
))

# value on tree levels for 1 time only
value(ptf,by=ad,type="nominal",tree=tree)
value(ptf,by=ad,type="markToModel",method=dcEngine,tree=tree)

# value on tree levels for multiple times
value(ptf,by=by,type="nominal",tree=tree)
value(ptf,by=by,type="markToModel",method=dcEngine,tree=tree)

# nominal income vector
income(ptf,by=by,type="marginal",revaluation.gains=FALSE,tree=tree)

# book-income vector
income(ptf,by=by,type="marginal",revaluation.gains=TRUE,tree=tree)
