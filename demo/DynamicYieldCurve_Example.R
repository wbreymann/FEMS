rm(list=ls())
library(FEMS)

devtools::load_all()


t <- "2012-12-31"
yc <- DynamicYieldCurve()
rates <- setNames(data.frame(t(c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03))),
                  c("1W", "1M", "6M", "1Y", "2Y", "5Y"))
rownames(rates) <- t
rates                  
yc <- DynamicYieldCurve(label = "YC_Prim", Rates = rates)
yc

# set function can be called as before...
set(yc, label = "YC_Prim", Rates = rates)
yc

rates2 <- setNames(data.frame(t(c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)))-0.001,
                  c("1W", "1M", "6M", "1Y", "2Y", "5Y"))
rownames(rates2) <- "2011-12-31"
rates2
add(yc, rates2)
yc

# test the implementation of the interest I get at 
rates(yc, to="2012-06-30", from="2011-12-31")
rates(yc, to="2012-06-30", from="2011-12-31")

# having dates overlapping two reference dates...
rates(yc, to="2013-05-31", from="2012-06-30")



# this is currently essentially the same as this...
(fwd <- rates(yc, to="2012-12-31", from="2012-06-30", refdate="2011-12-31"))
(fwd <- rates(yc, to="2012-12-31", from="2012-06-30", ad="2011-12-31"))
(spot <- rates(yc, to="2013-05-31", from="2012-12-31", refdate="2011-12-31")) # This is not a spot rate!

delta1 <- yearFraction("2012-06-30","2012-12-31",yc$DayCountConvention)
delta2 <- yearFraction("2012-12-31","2013-05-31",yc$DayCountConvention)
(fwd*delta1 + spot*delta2)/(delta1+delta2)

# another example with different refdate
rates(yc, to="2013-05-31", from="2012-06-30", refdate="2013-05-31")
fwd <- rates(yc, to="2012-12-31", from="2012-06-30")
spot <- rates(yc, to="2013-05-31", from="2012-12-31")
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










