
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
balance <- Node$new("FDIC")
balance$AddChild("Assets")
balance$Assets$AddChild("Liquidity")
balance$Assets$Liquidity$AddChild("Loan")
balance$Assets$Liquidity$AddChild("Mortgage")
balance$AddChild("Liabilities")
balance$Liabilities$AddChild("ShortTerm")
balance$Liabilities$ShortTerm$AddChild("Deposits")
balance$Liabilities$ShortTerm$AddChild("Bonds")
balance$AddChild("Equity")
balance$AddChild("Current")

balance

# Create current account and add to "Current"
# (Where should the current account go?)
# collector <- CurrentAccount(ContractID = "Collector", 
#                             CycleOfInterestPayment = "1Y-",
#                             CycleOfRateReset = "1Y-")
# addContracts(list(collector=collector), 
#              FindNode(balance, "Current"))

# fill the accounts with different contracts
# (Which accounts go where exactly?)
addContracts(ptf$contracts[1:2], FindNode(balance, "Loan"))
addContracts(ptf$contracts[6], FindNode(balance, "Mortgage"))
addContracts(ptf$contracts[c(3,5)], FindNode(balance, "Bonds"))

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

events(balance, t0, rf, end_date="2025-03-31")  # Error!!!
# Error: invalid assignment for reference class field ‘Data’, should be from class “timeSeries” or a subclass (was class “NULL”)

events(ptf$contracts[[1]], t0, rf)






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

  
  
  
  
  
  
  


