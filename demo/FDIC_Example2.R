#---------------------------------------------------
rm(list=ls())
devtools::load_all()


library(FEMS)

# load the data for PAMs and ANNs into a Portfolio
file.path.pam <- "./data-raw/PAM_FDIC.xls"
file.path.ann <- "./data-raw/ANN_FDIC.xls"

ptf <- Portfolio()
import(ptf, source = file.path.pam, sheet="Sheet1")
import(ptf, source = file.path.ann, sheet="Sheet1")

ptf

# arrange this into FDIC balance sheet structure 
# (This is not exactly the same as in the ARIADNE Example)
FDIC <- institution("FDIC")
FDIC$RemoveChild("PandL")
FDIC$Assets$RemoveChild("LongTerm")
FDIC

# FDIC$Assets$Current$contracts

# FDIC <- Node$new("FDIC")
# FDIC$AddChild("Assets")
# FDIC$Assets$AddChild("Liquidity")
FDIC$Assets$AddChild("Loan")
FDIC$Assets$AddChild("Mortgage")
# FDIC$AddChild("Liabilities")
# FDIC$Liabilities$AddChild("ShortTerm")
# FDIC$Liabilities$AddChild("Deposits")
FDIC$Liabilities$AddChild("Bonds")
# FDIC$AddChild("Equity")
# FDIC$AddChild("Current")

FDIC

# Create current account and add to "Current"
# (Where should the current account go?)
# collector <- CurrentAccount(ContractID = "Collector", 
#                             CycleOfInterestPayment = "1Y-",
#                             CycleOfRateReset = "1Y-")
# addContracts(list(collector=collector), 
#              FindNode(FDIC, "Current"))

# fill the accounts with different contracts
# (Which accounts go where exactly?)
addContracts(ptf$contracts[1:2], FindNode(FDIC, "Loan"))
addContracts(ptf$contracts[6], FindNode(FDIC, "Mortgage"))
addContracts(ptf$contracts[c(3,5)], FindNode(FDIC, "Bonds"))

FDIC
FDIC$Assets$Current$contracts
FDIC$Assets$Loan$contracts
FDIC$Assets$Mortgage$contracts
FDIC$Liabilities$Bonds$contracts

CTterms(ptf)
# (FindNode(FDIC, "Loan"))$isLeaf

get(ptf[[3]], "all")

# calculate events for the contracts
t0 <- "2019-03-31"

# just some generic Yield Curve
yc.tnr <- c("1M","10Y")
yc.rts <- c(0.02,0.02)
yc <- YieldCurve(label = "YC.USA.TREASURY",  
                 ReferenceDate = as.character(t0), 
                 Tenors = yc.tnr, 
                 Rates = yc.rts)

# Generate market environment
rf <- RFConn(yc)

# Generate "discounting engine"
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, rf)

events(FDIC, t0, rf, end_date="2025-03-31")  # Error!!!

FDIC$Assets$Current$eventList
FDIC$Assets$Loan$eventList
FDIC$Assets$Mortgage$eventList
FDIC$Liabilities$Bonds$eventList



events(ptf$contracts[[1]], t0, rf)
events(ptf$contracts[[2]], t0, rf)
events(ptf$contracts[[3]], t0, rf)
events(ptf$contracts[[4]], t0, rf)
events(ptf$contracts[[5]], t0, rf)
events(ptf$contracts[[6]], t0, rf)







######
# to have a working example, I transferred the existing bond portfolio sheet to the new 
# ACTUS version

file.path <- "./data/BondPortfolio.xls"

ptf <- Portfolio()
import(ptf, source = file.path, sheet="BondPortfolio")

t0 <- "2015-01-01"

# just some generic Yield Curve
yc.tnr <- c("1M","10Y")
yc.rts <- c(0.02,0.02)
yc <- YieldCurve(label = "YC_EA_AAA", 
                 ReferenceDate = as.character(t0), 
                 Tenors = yc.tnr, 
                 Rates = yc.rts)

# Generate market environment
rf <- RFConn(yc)

# Generate "discounting engine"
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, rf)
events(ptf, t0, rf)

  
  
  
  
  
  
  


