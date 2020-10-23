###########################################################
# Define empty contract & print
bc <- BaseContract()
bc

# Define example contract & print
bc1 <- BaseContract(Dates=c("2019-12-31","2020-12-31"),
                    CashFlows=c(-90,100))
bc1
# Why should we need those commands? is just a normal print of the contract?
# cashFlows(bc1, "2019-12-31")  ## Error!
# events(bc1, "2019-12-31") ## Error!

bc1 <- BaseContract(Dates=c("2019-12-31","2020-12-31"),
                    CashFlows=c(-90,100))
bc1
# cashFlows(bc1, "2019-12-31")  ## Error!


# Contract with variable time
bc2 <- BaseContract(Dates=c("2019-12-31","2020-12-31","tt"),
                    CashFlows=c(-90,100,10))
bc2
as.Date(bc2$Dates)
# Since the variable cannot be transformed into a "Date", we get "NA"
# which can be used to extract the variable:
# bc2$Dates[is.na(as.Date(bc2$Dates))]
# eval(as.name(bc2$Dates[is.na(as.Date(bc2$Dates))]))  ## Error!!

###########################################################
# test the implementation of the discountFactorsv2

yc <- YieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(
  label = "YC_Prim",
  ReferenceDate = "2015-01-01",
  Tenors = tenors,
  Rates = rates))

# calculate some discount factors (and compare to previous function)
discountFactors(yc, by="1Y")
discountFactors(yc, to="2016-01-01")
discountFactors(yc, by="1Y", from="2015-07-01")
discountFactors(yc, to="2016-07-01", from="2015-07-01")

###########################################################
# test the calculation of the value function

bc2 <- BaseContract(Dates=c("2016-01-01","2017-01-01"),
                    CashFlows=c(-95,100))
# value before the first cash flow
vals_bef <- value(bc2, by = "2015-01-01", curve = yc) 
vals_at <- value(bc2, by = "2017-01-01", curve = yc) 
vals_after <- value(bc2, by = "2018-01-01", curve = yc) 

rbind(vals_bef, vals_at, vals_after)

# value before reference date of yield curve (should return error)
# vals_err <- value(bc2, by = "2014-01-01", curve = yc)


###########################################################
# Define a flat yield curve

(yc_flat <- MarketInterestRate(0.05, "2015-01-01"))
(val <- value(bc2, by="2016-01-01", curve = yc_flat))

# discountFactor function with flat yield curve...
discountFactors(yc_flat, by="1Y", from="2015-01-01") # default method is "continuous"
discountFactors(yc_flat, by="1Y", from="2015-01-01", method="linear")
discountFactors(yc_flat, by="1Y", from="2015-01-01", method="compound")
discountFactors(yc_flat, by="1Y", from="2015-01-01", method="compound", period="H")
discountFactors(yc_flat, by="1Y", from="2015-01-01", method="continuous")

# use multiple dates
discountFactors(yc_flat, to=c("2016-01-01","2017-01-01"), 
                           from=c("2015-01-01","2016-01-01"))

discountFactors(yc_flat, to=c("2016-06-30","2027-12-31"), 
                           form=c("2015-01-01","2016-01-01"), method = "linear")

discountFactors(yc_flat, to=c("2015-01-01","2016-01-01"), 
                           from=c("2016-06-30","2027-12-31"), method = "linear")

(app.fact.w = discountFactors(yc_flat, to=c("2015-01-01","2016-01-01"), 
                           from=c("2016-06-30","2027-12-31"), method = "compound", period = "W"))
# Test of first example
# The test implies A/A dcc:
(1+0.05/52.14)^78
((1+0.05/52.14)^78 - app.fact.w[1])/app.fact.w[1]

(app.fact.cont = discountFactors(yc_flat, to=c("2015-01-01","2016-01-01"), 
                           from=c("2016-06-30","2027-12-31"), method="continuous"))
# Test
exp(0.05*(364+31+29+31+30+31+30)/365)
# relative error
(exp(0.05*(364+31+29+31+30+31+30)/365) - app.fact.cont[1])/app.fact.cont[1]


eng <- DcEngine(dc.object=yc_flat, dc.spread=0)
class(b1)
ev <- events(b1, "2015-01-01")

value(b1, "2015-01-01", type="markToModel", method=eng)
value(ev, "2015-01-01", type="markToModel", method=eng)
value(b1, "2015-01-01", type="nominal")
value(ev, "2015-01-01", type="nominal")
