#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

rm(list= ls())

#-------------------------------------------------------------------
# load rActus library
#-------------------------------------------------------------------
#library(rActus)

#-------------------------------------------------------------------
# create a YieldCurve risk factor object
#-------------------------------------------------------------------
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  label = "YC_Prim",
  ReferenceDate = "2015-01-01", 
  Tenors = tenors, 
  Rates = rates))

# retrieve yield curve parameters ERROR: Does no longer work!!!
get(yc, "label")
get(yc, "ReferenceDate")
get(yc, "Tenors")
get(yc, "Rates")

# access yield curve functions
rates(yc, by="1Y")  # 1-year spot rate
rates(yc, to="2016-01-01") # again, 1-year spot rate
rates(yc, by="1Y", from="2015-07-01")  # 1-year forward rate at 2015-07-01
discountFactors(yc, by="1Y")
discountFactors(yc, to="2016-01-01")
discountFactors(yc, by="1Y", from="2015-07-01")

# plot yield curve
plot(yc)

#-------------------------------------------------------------------
# create a Reference Index risk factor object
#-------------------------------------------------------------------

times <- timeSequence(from="2014-01-01", by="3 months", 
                      length.out=9)
values <- cumsum(c(1,rnorm(8,0.02,0.1)))
idx <- Index(label = "PriceIndex", 
             data = values, 
             charvec = times)

# retrieve reference index parameters
get(idx, "label")
get(idx, "Data")

# access reference index functions
valueAt(idx, "2016-01-01")
valueAt(idx, c("2016-01-01", "2016-07-01", "2017-01-01"))

# plot index
plot(idx)


#-------------------------------------------------------------------
# create a connector instance linking contracts and risk factors
#-------------------------------------------------------------------
rf <- RFConn()
add(rf,list(yc,idx))
get(rf,"keys")
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
remove(rf, "YC_Prim")
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
add(rf,list(yc))
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
