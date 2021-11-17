library(FEMS)

rm(list=ls())
devtools::load_all()

# create simple fixed rate bond instruments
bnd1=bond(start="2015-01-01", maturity="30 years", nominal=1000, 
          coupon=0.06, couponFreq="1 year", role="long", variable=FALSE)


bnd2=bond(start="2015-01-01", maturity="10 years", nominal=1000, 
          coupon=0.11, couponFreq="1 year", role="long", variable=FALSE)
bnd3=bond(start="2015-01-01", maturity="20 years", nominal=1000, 
          coupon=0.09, couponFreq="1 year", role="long", variable=FALSE)

b1 <- bond(start = "2020-01-01", maturity = "5 years", nominal = 10000, 
           coupon = 0.05, ContractID="001")
a1 <- annuity("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
              , ContractID="002")
l1 <- loan("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
           , ContractID="003")

ptf = Portfolio(b1=bnd1,b2=bnd2,b3=bnd3)
ptf
# FEMS:::get(ptf, "contracts")

cashFlows(bnd1)
cashFlows(bnd2)
cashFlows(bnd3)
cashFlows(ptf)

presentValue(bnd1, yield=9)
presentValue(bnd2, yield=9)
presentValue(bnd3, yield=9)
presentValue(ptf, yield=c(9,9,9))

presentValue(bnd1, yield=9, isPrice=TRUE)
presentValue(bnd2, yield=9, isPrice=TRUE)
presentValue(bnd3, yield=9, isPrice=TRUE)
presentValue(ptf, yield=c(9,9,9), isPrice=TRUE)


t <- "2015-01-01"
yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)

# set function can be called as before...
set(yc, what = list(
  label = "YC_Prim",
  ReferenceDate = t,
  Tenors = tenors,
  Rates = rates))

presentValue(ptf, yieldCurve=yc, isPrice=TRUE)

duration(bnd1, type="macaulay", yield=9)
duration(bnd2, type="macaulay", yield=9)
duration(bnd3, type="macaulay", yield=9)
duration(bnd3, type="macaulay", yield=9, digits=1)
duration(ptf, type="macaulay", yield=c(9,9,9))
duration(ptf, type="macaulay", yield=c(9,9,9),digits=1)

# compute all possible portfolios consisting of two assets (bonds) which
# immunize a future payment obligation, here the 'target'
target=bond(start="2015-01-01", maturity="10 years", nominal=1000000, coupon=0,
            couponFreq="10 year", role="long", variable=FALSE)
res=immunize(ptf, target, yield=9, from=NULL)
res

# check for the two resulting portfolios
# 1. whether present value of target and immunizing portfolio assets match
presentValue(target, yield=9, isPrice=TRUE, digits=0)
sum(res[[1]]$values)
sum(res[[2]]$values)

# 2. whether duration of target and immunizing portfolio assets match
duration(target, yield=9)
res[[1]]$values[1]/sum(res[[1]]$values)*duration(bnd2, type="macaulay", yield=9)+
  res[[1]]$values[2]/sum(res[[1]]$values)*duration(bnd1, type="macaulay", yield=9)
res[[2]]$values[1]/sum(res[[2]]$values)*duration(bnd3, type="macaulay", yield=9)+
  res[[2]]$values[2]/sum(res[[2]]$values)*duration(bnd1, type="macaulay", yield=9)

