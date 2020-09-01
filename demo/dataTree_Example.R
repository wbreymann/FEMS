
#setwd("rActus")
library("devtools")
devtools::load_all()

# options(warn=-1)
# library(rActus)
# library(rflPortfolio)
# library(data.tree)
# source("./R/DataTreeFunctions.R")

# Define example contract & print
bc1 <- BaseContract(Dates=c("2013-12-31"),
                    CashFlows=c(150000))
bc1

# Contract with variable time
bc2 <- BaseContract(
  Dates=c("2014-01-01","2015-01-01","2016-01-01","2017-01-01","2018-01-01"),
  CashFlows=rep(-6000,5))
bc2

# The portfolio is just a list of contracts, a container:
ptf = list(bc1, bc2)
# The names should be constructed automatically, cf. ptfPortfolio
names(ptf) = c("bc1", "bc2") 
ptf[["bc1"]]
ptf["bc1"]

# Analysis structure
BS = Node$new("Portfolio")
BS$AddChild("Wealth")
BS$AddChild("Payouts")
BS$Wealth$ctNames = c("bc1")
BS$Payouts$ctNames = c("bc2")

# Remark:
# For simple problems there is no need to separate the container of contracts 
# and the analysis structure.
# We should thing about how we can do this.
# In any case, this would require modifying the functions "fAnalytics" and 
# "aggregateAnalytics" in DataTreeFunctions.R

BS
BS$Wealth$ctNames
BS$Payouts$ctNames

BS$children
class(BS$children)

# yield curve
(yc_flat <- MarketInterestRate(0.025, "2013-12-31"))
yc_flat
(val1 <- value(bc1, by="2023-12-31", curve=yc_flat, compound="compound")) 
(val2 <- value(bc2, by="2023-12-31", curve=yc_flat, compound="compound")) 


# (val1 <- value(bc1, by=c("2023-12-31","202-12-31"), curve=yc_flat, compound="compound")) 


tt = "2018-12-31"
# And now for value:
### The following does not longer work.
clearAnalytics(BS, "value")   ## function not found
BS$Do(fun=fAnalytics, "value", ptf, by=tt, curve=yc_flat, compound="compound", 
      filterFun=isLeaf)
aggregateAnalytics(BS, "value")

BS$Wealth$value
BS$Payouts$value
clearAnalytics(BS, "value")
BS$Wealth$value
BS$Payouts$value

#---------------------------------------------------------------
# Here the analysis structure consists of a single node, only
BS2 = Node$new("Portfolio2")
BS2$ctNames = c("bc1", "bc2")
BS2
BS2$ctNames
clearAnalytics(BS2, "value")
BS2$Do(fun=fAnalytics, "value", ptf, by=tt, curve=yc_flat, compound="compound", 
      filterFun=isLeaf)
BS2$value
BS2[["value"]]
aggregateAnalytics(BS2, "value")

