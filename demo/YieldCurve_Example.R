#####################################################
# YieldCurve - Object

rm(list=ls())
library(FEMS)

t <- "2012-12-31"
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)

# set function can be called as before...
set(yc, what = list(
  label = "YC_Prim",
  ReferenceDate = t,
  Tenors = tenors,
  Rates = rates))

#####################################################
# YieldCurve object now has the following fields
# MarketObjectCode
# ReferenceDate
# Tenors
# Rates
# TenorDates
# DayCountConvention
names(yc)

#####################################################
# When showing the YieldCurve, we would see the following fields only:
# MarketObjectCode
# ReferenceDate
# DayCountConvention
# Curve (which is showing the tenors and the rates)
show(yc)

# plot function as before
plot(yc)

# get rates & discount factor from relative date
rates(yc, by="1Y")
discountFactors(yc, by="1Y")

# get 1 year forward rates as of specific dates
rates(yc, by="1Y", from="2013-06-30")
discountFactors(yc, by="1Y", from="2013-06-30")

# change day count convention
set(yc, what = list(DayCountConvention = "A365"))
yc$DayCountConvention <- "A365" # or same as this...


# get rates and discount factors
rates(yc, by="6M", from="2013-06-30")
discountFactors(yc, by="6M", from="2013-06-30")


# get a weekly (default) series of 1M (default) forward rates between two dates
getRateSeries(yc, startdate="2012-12-31", enddate="2013-12-31")  # Please add argument for time step


##############################################################################
# another example with creating forward rate series
yc2 <- YieldCurve(label = "YC_Prim",
                  ReferenceDate = "2012-12-31",
                  Tenors = c("1M", "2M", "3M", "4M", "5M", "6M"),
                  Rates = c(0.01, 0.015, 0.02, 0.022, 0.024, 0.025))

fwd_rates <- getRateSeries(yc2, startdate="2012-12-31", enddate="2013-06-30", frequency = "month")
fwd_rates

# now change day count convention
yc2$DayCountConvention <- "A365"
fwd_rates2 <- getRateSeries(yc2, startdate="2012-12-31", enddate="2013-06-30", frequency = "month")
fwd_rates2

# or get a weekly series
fwd_rates3 <- getRateSeries(yc2, startdate="2012-12-31", enddate="2013-06-30", frequency = "week")
fwd_rates3
plot(fwd_rates3$Values)


# 1M forward and weekly series is default, but can also be flexible; ex.: 2M fwd, daily
fwd_rates4 <- getRateSeries(yc2, startdate="2012-12-31", enddate="2013-06-30", frequency = "day", forward = "2M")
plot(fwd_rates4$Values)

