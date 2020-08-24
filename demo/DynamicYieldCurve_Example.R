devtools::load_all()
rm(list=ls())


t <- "2012-12-31"
yc <- DynamicYieldCurve()
rates <- setNames(data.frame(t(c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03))),
                  c("1W", "1M", "6M", "1Y", "2Y", "5Y"))
rownames(rates) <- t
                  
# set function can be called as before...
set(yc, what = list(
  MarketObjectCode = "YC_Prim",
  Rates = rates))

rates2 <- setNames(data.frame(t(c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)))-0.001,
                  c("1W", "1M", "6M", "1Y", "2Y", "5Y"))
rownames(rates2) <- "2011-12-31"
add(yc, rates2)
yc

# test the implementation of the interest I get at 
rates(yc, "2012-06-30", "2011-12-31", isDateEnd=TRUE)

# having dates overlapping two reference dates...
rates(yc, "2013-05-31", "2012-06-30", isDateEnd=TRUE)

# this is currently essentially the same as this...
yc
fwd <- rates(yc, "2012-12-31", "2012-06-30", isDateEnd=TRUE)
spot <- rates(yc, "2013-05-31", "2012-12-31", isDateEnd=TRUE)
delta1 <- yearFraction("2012-06-30","2012-12-31",yc$DayCountConvention)
delta2 <- yearFraction("2012-12-31","2013-05-31",yc$DayCountConvention)
(fwd*delta1 + spot*delta2)/(delta1+delta2)





