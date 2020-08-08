# this is to test the connection with the new web server...
rm(list=ls())
devtools::load_all()


# create a PAM & set attributes...
pam <- Pam()
set(pam, what = list(
    Calendar = "MondayToFriday",
    BusinessDayConvention = "SCF",
    ContractType = "PAM",
    StatusDate = "2015-01-01",
    ContractRole = "RPA",
    ContractID = "Contract-03",
    CycleAnchorDateOfInterestPayment = "2016-01-02",
    CycleOfInterestPayment = "1Y+",
    NominalInterestRate = 0.00,
    DayCountConvention = "30E/360",
    Currency = "USD",
    ContractDealDate = "2015-01-01",
    InitialExchangeDate = "2015-01-02",
    MaturityDate = "2020-01-02",
    NotionalPrincipal = 1000,
    RateSpread = 0,
    PremiumDiscountAtIED = 0,
    CycleAnchorDateOfRateReset = "2016-01-02",
    CycleOfRateReset = "1Y+",
    # RateSpread = 0.02,
    RateMultiplier = 1,
    MarketObjectCodeRateReset = "YC.USA.TREASURY"))

# show method:
pam
# I don't want to have the attributes returned in list format, it's confusing.
# But we may make it nicer.

# Summary
summary(pam)
# We may add some useful information but it's a bad idea to simple return the
# list of attributes

# Subsetting Operators:
pam[c(1,3,5)]
pam[35]
pam["NotionalPrincipal"]
pam[c("ContractType", "ContractID", "NotionalPrincipal")]

# Replacement operators
pam[35] <- 100000
pam[35]
pam["NotionalPrincipal"] = 1000
pam[35]

# create yield curve
yc <- YieldCurve(MarketObjectCode = "YC.USA.TREASURY",
                 ReferenceDate = "2015-01-01",
                 Tenors = c("1M", "2M", "3M", "4M", "5M", "6M","1Y","5Y"),
                 Rates = c(0.01, 0.015, 0.02, 0.022, 0.024, 0.025, 0.03, 0.035))

# set the simulation through time based on the yield curve from refdate to today
# Q bwlf: 
# Which date is "today"?
setTimeSeries(yc, yc$ReferenceDate, as.character(today()))

# create a risk factor connector & add YieldCurve
rf <- RFConn()
add(rf, yc)

# set the riskfactor connector of the PAM
set(pam, rf)

# calculate events via API
evs <- events(pam, "2015-01-01")

value(pam, by = "2015-01-02", type = "nominal")
eng <- DcEngine(RiskFactorObject = yc, DiscountingSpread = 0.0)
value(pam, by = "2015-01-02", type = "markToModel", method = eng)


######Now do this on a portfolio level...
pam2 <- Pam()
set(pam2, what = list(
  Calendar = "MondayToFriday",
  BusinessDayConvention = "SCF",
  ContractType = "PAM",
  StatusDate = "2015-01-01",
  ContractRole = "RPA",
  ContractID = "Contract-05",
  CycleAnchorDateOfInterestPayment = "2016-01-02",
  CycleOfInterestPayment = "1Y+",
  NominalInterestRate = 0.00,
  DayCountConvention = "30E/360",
  Currency = "USD",
  ContractDealDate = "2015-01-01",
  InitialExchangeDate = "2015-01-02",
  MaturityDate = "2020-01-02",
  NotionalPrincipal = 1000,
  RateSpread = 0,
  PremiumDiscountAtIED = 0,
  CycleAnchorDateOfRateReset = "2016-01-02",
  CycleOfRateReset = "1Y+",
  RateMultiplier = 1,
  MarketObjectCodeRateReset = "YC.USA.TREASURY"))

# Create a portfolio
port <- Portfolio()

# add the two contracts separately
add(port, pam)
add(port, pam2)

# set riskfactor conector
set(port, rf)

# calculat events via API
evs_port <- events(port, "2015-01-01")

# calculate value of portfolio
value(port, "2015-01-02", "markToModel", method = eng)
value(port, "2015-01-02", type = "nominal")

# for multiple times
by <- timeSequence(substring("2015-01-02",1,10), "2020-06-01", by = "1 year")
value(port, by, type = "nominal")

## compute mark-to-model value
value(port, by, type = "markToModel", method = eng)

# calculate liquidity
liquidity(port, by = by, type = "marginal")
liquidity(port, by = by, type = "cumulative")

# nominal income vector
income(port, by = by, type = "marginal", revaluation.gains = FALSE)
income(port, by = by, type = "marginal", revaluation.gains = TRUE, method = eng)




##### This is to replicate the call that was sent to Postman to test the API initially...
# json_body <- read_json("./data-raw/sample_data.json")
# json_body <- toJSON(json_body, pretty = TRUE, auto_unbox = TRUE)
# response_events <- POST("http://ractus.ch:8080/eventsBatch", body = json_body, content_type_json())
# response_content <- content(response_events)

##### This is interesting behaviour when dropping the CallNextMethod() function in initialize within Pam...
##### Seems dangerous not to include it into any initialize function...
# pam1 <- Pam()
# set(pam1, what = list(ContractID = "Test"))
# pam1$attributes$ContractID
# pam2 <- Pam()
# pam1$attributes$ContractID

