#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

rm(list= ls())

#-------------------------------------------------------------------
# load rActus library
#-------------------------------------------------------------------
library(rActus)

#-------------------------------------------------------------------
# create a YieldCurve risk factor object
#-------------------------------------------------------------------
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))

# retrieve yield curve parameters
get(yc, "MarketObjectCode")
get(yc, "ReferenceDate")
get(yc, "Tenors")
get(yc, "Rates")

# access yield curve functions
rates(yc, "1Y")  # 1-year spot rate
rates(yc, "2016-01-01T00", isDateEnd=TRUE) # again, 1-year spot rate
rates(yc, "1Y", "2015-07-01T00")  # 1-year forward rate at 2015-07-01T00
discountFactors(yc, "1Y")
discountFactors(yc, "2016-01-01T00", isDateEnd=TRUE)
discountFactors(yc, "1Y", "2015-07-01T00")

# plot yield curve
plot(yc)

#-------------------------------------------------------------------
# create a Reference Index risk factor object
#-------------------------------------------------------------------
ind <- Index()
times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", "2018-01-01T00",
           "2019-01-01T00")
values <- c(100, 110, 120, 130, 140)
set(ind, what=list(
  MarketObjectCode = "CHF_SMI",
  Data=list(Dates=times,Values=values)))

# retrieve reference index parameters
get(ind, "MarketObjectCode")
get(ind, "Data")

# access reference index functions
valueAt(ind, "2016-01-01T00")
valueAt(ind, c("2016-01-01T00", "2016-07-01T00", "2017-01-01T00"))

# plot index
plot(ind)

#-------------------------------------------------------------------
# create a Foreign Exchange Rate risk factor object
#-------------------------------------------------------------------
fx <- FxRate()
times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", 
           "2018-01-01T00", "2019-01-01T00")
values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
set(fx, what=list(
  MarketObjectCode = "CHF/USD",
  Data=list(Dates=times,Values=values)))

# retrieve reference index parameters
get(fx, "MarketObjectCode")
get(fx, "Data")

# access reference index functions
valueAt(fx, "2016-01-01T00")
valueAt(fx, c("2016-01-01T00", "2018-07-01T00", "2018-07-01T00"))

# plot fx-series
plot(fx)

#-------------------------------------------------------------------
# create a connector instance linking contracts and risk factors
#-------------------------------------------------------------------
rf <- RFConn()
add(rf,list(yc,ind,fx))
get(rf,"keys")
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
remove(rf, "YC_Prim")
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
add(rf,list(yc,ind,fx))
remove(rf, c("YC_Prim", "CHF_SMI"))
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
