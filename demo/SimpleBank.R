
library(FEMS)

rm(list=ls())
devtools::load_all()

# load the data for PAMs and ANNs into a Portfolio
file.path.pam <- "./data/PAM_FDIC.xls"
file.path.ann <- "./data/ANN_FDIC2.xls"

ptf <- Portfolio()
import(ptf, source = file.path.pam, sheet="Sheet1")
import(ptf, source = file.path.ann, sheet="Sheet1")



ptf[[3]]
terms(ptf[[3]])

# set rate reset by hand
set(object = ptf[[3]], what = list(
  CycleAnchorDateOfRateReset = "2020-03-31",
  CycleOfRateReset = "P6ML0", # 
  MarketObjectCodeOfRateReset = "YC.USA.TREASURY"))
ptf[[3]]$ContractTerms

# save(ptf, file = "SimpleBank.RData")
load("./data/SimpleBank.RData")

# arrange this into SimpleBank balance sheet structure 
# (This is not exactly the same as in the ARIADNE Example)
SimpleBank <- institution("SimpleBank")
# SimpleBank$RemoveChild("PandL")
# SimpleBank$Assets$RemoveChild("LongTerm")

SimpleBank$Assets$LongTerm$AddChild("Loan")
SimpleBank$Assets$LongTerm$AddChild("Mortgage")
SimpleBank$Liabilities$AddChild("Bonds")
SimpleBank$AddChild("Equity")

SimpleBank

# Create current account and add to "Current"
# (Where should the current account go?)
# collector <- CurrentAccount(ContractID = "Collector", 
#                             CycleOfInterestPayment = "1Y-",
#                             CycleOfRateReset = "1Y-")
# addContracts(list(collector=collector), 
#              FindNode(SimpleBank, "Current"))

# fill the accounts with different contracts
# (Which accounts go where exactly?)
addContracts(ptf$contracts[1:2], FindNode(SimpleBank, "Loan"))
addContracts(ptf$contracts[6], FindNode(SimpleBank, "Mortgage"))
addContracts(ptf$contracts[c(3,5)], FindNode(SimpleBank, "Bonds"))

SimpleBank
SimpleBank$Assets$Current$contracts
SimpleBank$Assets$LongTerm$Loan$contracts
SimpleBank$Assets$LongTerm$Mortgage$contracts
SimpleBank$Liabilities$Bonds$contracts

# (FindNode(SimpleBank, "Loan"))$isLeaf

# calculate events for the contracts
t0 <- "2020-02-28"

# just some generic Yield Curve
yc.tnr <- c("1W", "1M", "6M", "1Y", "2Y", "5Y", "10Y", "20Y")
yc.rts <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03, 0.045, 0.06)
yc <- YieldCurve(label = "YC.USA.TREASURY",  
                 ReferenceDate = as.character(t0), 
                 Tenors = yc.tnr, 
                 Rates = yc.rts)

# Generate market environment
rf <- RFConn(yc)

# Generate "discounting engine"
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, rf)

events(SimpleBank, t0, rf, end_date="2035-03-31")  # Error!!!

SimpleBank$Assets$Current$eventList
SimpleBank$Assets$Loan$eventList
SimpleBank$Assets$Mortgage$eventList
SimpleBank$Liabilities$Bonds$eventList


by <- timeSequence(t0, by="1 years", length.out=16)

tb <- timeBuckets(by, bucketLabs=2020:2034, breakLabs=substr(as.character(by),3,10))  
tb

scale=1000
liquidity(SimpleBank, tb, scale=scale, digits=2)

devtools::load_all()

# Income (in kEUR)
income(SimpleBank, tb, scale=scale, digits=2)

# Nominalwert (in kEUR) (is default)
# value(SimpleBank, tb, "nominal", scale=scale, digits=2)
value(SimpleBank, tb, scale=scale, digits=2)
SimpleBank$Liabilities$Equity$value <- SimpleBank$value
FEMS:::aggregateAnalytics(SimpleBank, "value")

SimpleBank$Liabilities$value <- SimpleBank$Liabilities$Bonds$value + SimpleBank$Liabilities$Equity$value


# market valuation
# round(value(SimpleBank, tb, "market", method=diskont)/scale,2)
value(SimpleBank, tb, "market", method=diskont, scale=scale, digits=2)

