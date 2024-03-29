# this is to test the connection with the new web server...
rm(list=ls())
devtools::load_all()


# create a PAM & set ContractTerms...
pam <- Pam()
set(pam, what = list(
    Calendar = "MF",
    BusinessDayConvention = "SCF",
    ContractType = "PAM",
    StatusDate = "2015-01-01",
    ContractRole = "RPA",
    ContractID = "Contract-03",
    CycleAnchorDateOfInterestPayment = "2016-01-02",
    CycleOfInterestPayment = "P1YL0",
    NominalInterestRate = 0.00,
    DayCountConvention = "30E360",
    Currency = "USD",
    ContractDealDate = "2015-01-01",
    InitialExchangeDate = "2015-01-02",
    MaturityDate = "2020-01-02",
    NotionalPrincipal = 1000,
    RateSpread = 0,
    PremiumDiscountAtIED = 0,
    CycleAnchorDateOfRateReset = "2016-01-02",
    CycleOfRateReset = "P1YL0",
    RateMultiplier = 1,
    MarketObjectCodeOfRateReset = "YC.USA.TREASURY"))

# show method:
pam

# I don't want to have the ContractTerms returned in list format, it's confusing.
# But we may make it nicer.

# Summary
summary(pam)
# We may add some useful information but it's a bad idea to simple return the
# list of ContractTerms

# Zugriff auf Elemente:
get(pam, "ContractID")
get(pam, "ContractID", "ContractType") ## Error

get(pam, c("ContractID", "ContractType")) ## ok
get(pam, list("ContractID", "ContractType")) ## Error
get(pam, "all")  # Dies sollte auch funktionieren, aber nur die Contract Terms oder alles?

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
yc <- YieldCurve(label = "YC.USA.TREASURY",
                 ReferenceDate = "2015-01-01",
                 Tenors = c("1M", "2M", "3M", "4M", "5M", "6M","1Y","5Y"),
                 Rates = c(0.01, 0.015, 0.02, 0.022, 0.024, 0.025, 0.03, 0.035))

# create a risk factor connector & add YieldCurve
rf <- RFConn()
add(rf, yc)

# set the riskfactor connector of the PAM
set(pam, rf)

# calculate events via API
evs <- events(pam, "2015-01-01")

value(pam, by = "2015-01-02", type = "nominal")
eng <- DcEngine(dc.object = yc, dc.spread = 0.0)
value(pam, by = "2015-01-02", type = "markToModel", method = eng)

plot(pam,"2015-01-01")

######Now do this on a portfolio level...
pam2 <- Pam()
set(pam2, what = list(
  Calendar = "MF",
  BusinessDayConvention = "SCF",
  ContractType = "PAM",
  StatusDate = "2015-01-01",
  ContractRole = "RPA",
  ContractID = "Contract-05",
  CycleAnchorDateOfInterestPayment = "2016-01-02",
  CycleOfInterestPayment = "P1YL0",
  NominalInterestRate = 0.00,
  DayCountConvention = "30E360",
  Currency = "USD",
  ContractDealDate = "2015-01-01",
  InitialExchangeDate = "2015-01-02",
  MaturityDate = "2020-01-02",
  NotionalPrincipal = 1000,
  RateSpread = 0,
  PremiumDiscountAtIED = 0,
  CycleAnchorDateOfRateReset = "2016-01-02",
  CycleOfRateReset = "P1YL0",
  RateMultiplier = 1,
  MarketObjectCodeOfRateReset = "YC.USA.TREASURY"))

# Create a portfolio
port <- Portfolio()

# add the two contracts separately
# Comment: Hier wäre die Syntax port$add(pam) angebracht
add(port, pam)
add(port, pam2)
# Folgendes sollte auch möglich sein: port$add(list(pam, pam2))

get(pam2, "ContractID")

summary(port)
port

# Different ways to assess contract terms
CTterms(port, "Contract-03", c("ContractID","ContractType"))
CTterms(port, 1:2, c("ContractID","ContractType"))
CTterms(port, , c("ContractID","ContractType"))

length(CTterms(port, ,"ContractID"))
CTterms(port, ,"ContractID")
CTterms(port, ,"ContractID")[,1,TRUE]
dim(CTterms(port, ,"ContractID"))

port["Contract-03"] # Returns selected variables as data.frame
port[1] # as before
port[["Contract-03"]] # Returns all contract terms as a list, because "[[" has not been overwritten
port[[1]] # As before

port[c("Contract-03","Contract-03")]
port[c("Contract-03","Contract-05")]
port[c("Contract-03","Contract-05"),]
# port[c("Contract-03","Contract-05"),1] # Fehler, as it should be
port[c("Contract-03","Contract-05"),"ContractID"] # Warum Fehler? 


# set riskfactor conector
set(port, rf)

# calculat events via API
evs_port <- events(port, "2015-01-01")

evs_port
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















