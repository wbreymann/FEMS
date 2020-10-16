rm(list=ls())
library(FEMS)

t <- "2012-12-31"
yc <- DynamicYieldCurve()
rates <- setNames(data.frame(t(c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03))),
                  c("1W", "1M", "6M", "1Y", "2Y", "5Y"))
rownames(rates) <- t
                  
# set function can be called as before...
set(yc, what = list(
  label = "YC_Prim",
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
fwd <- rates(yc, "2012-12-31", "2012-06-30", isDateEnd=TRUE, refdate="2011-12-31")
spot <- rates(yc, "2013-05-31", "2012-12-31", isDateEnd=TRUE, refdate="2011-12-31")
delta1 <- yearFraction("2012-06-30","2012-12-31",yc$DayCountConvention)
delta2 <- yearFraction("2012-12-31","2013-05-31",yc$DayCountConvention)
(fwd*delta1 + spot*delta2)/(delta1+delta2)

# another example with different refdate
rates(yc, "2013-05-31", "2012-06-30", isDateEnd=TRUE, refdate="2013-05-31")
fwd <- rates(yc, "2012-12-31", "2012-06-30", isDateEnd=TRUE)
spot <- rates(yc, "2013-05-31", "2012-12-31", isDateEnd=TRUE)
(fwd*delta1 + spot*delta2)/(delta1+delta2)



# Build another dynamic yield curve (used in exercises)...
yld.strt <- c(0.06, 0.06, 0.1)
yld.reduce <- seq(0.00, 0.02, 0.005)

yld <- t(sapply(yld.reduce, function(x) yld.strt-x))
yld <- rbind(yld, 
             yld[nrow(yld),]+0.02, 
             yld[nrow(yld),]+0.02)
yld <- data.frame(yld)
rownames(yld) <- c("2012-12-31","2013-06-30",
                   "2013-12-31","2014-06-30",
                   "2014-12-31","2015-06-30",
                   "2015-12-31")
colnames(yld) <- c("1D","10Y","30Y")
(yc <- DynamicYieldCurve(Rates = yld))










