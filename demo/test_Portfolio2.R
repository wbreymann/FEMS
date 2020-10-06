rm(list=ls())
devtools::load_all()

b1 <- bond(start = "2020-01-01", maturity = "5 years", nominal = 10000, 
           coupon = 0.05, ContractID="001")
a1 <- annuity("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
              , ContractID="002")
l1 <- loan("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
           , ContractID="003")


p1 <- Portfolio2(b1,a1)
p1
names(p1)
all.equal(p1[[1]], p1[[names(p1)[1]]])
length(p1[c("001","002")])
class(p1["001"])
p1[["001"]]

p2 <- c(p1, l1)
p2
length(p2)

p3 <- Portfolio2(l1)
class(p3)
p3

pc <- c(p1, p3)
pc
class(pc)
length(pc)

class(pc[[1]])
all.equal(pc[[1]], pc[["001"]])



## ---- bank account -----------------------------------------------------------
(cashflows <- timeSeries(
  c(1000,1000,30000,10000,-20000), units="CHF",
  timeSequence(from="2014-04-01", by="year", length.out=5)))

(my.account <- bankAccount("2013-12-31", balance=50000,
                           ext_transactions = cashflows, ir=0.02))

## ---- Operational cash flows -------------------------------------------------
(times = timeSequence(from="2014-01-01", by="3 months", length.out=9))
values = cumsum(c(1,rnorm(8,0.02,0.1)))
# Creating the market index
idx <- Index(label = "PriceIndex", data = values, 
             charvec = times)
# function for revenue
revenue <- function(idx, times) { 
  idx$Data[as.character(times),] * 1000
}
# Contructing object for operational cf
OpCFs <- OperationalCF(
  ContractID="Ops001", Currency="CHF",
  pattern = revenue, # the function
  args = list( # the argument of the function
    idx = idx,  
    times = as.character(times)
  )
)
## ---- Investment -------------------------------------------------------------
# function generating the write-offs
write.off <- function(times) {
  timeSeries(seq(1000000, 0, length.out=9), times) 
}
# Creating the investment object 
invest <- Investments(
  ContractID = "Ops002", Currency = "CHF", 
  pattern = write.off, 
  args = list(times = times))


Portfolio2(b1, invest)  # Error because of divergent structure of contracts

#' The following should be valid (I write it in a way that it can 
#' be used directly for the help):
#' 
#' All contracts are derived from a (virtual/abstract) base class 
#' \code{ContractABC}. (or similar)
#'  
#' Each contract has a unique \code{ContractID}
#' The method \code{ctnames} returns the \code{ContractID}
#'  
#'  The field \code{contracts} of the class \code{Portfolio} contains a list 
#'  of contracts.
#'  The names of the contracts are their \code{ContractID}s.
#'  These names are set when an object of class \code{Portfolio} is created
#'  or when a contract is added to the portfolio.
#'  
#'  The \code{ContractID} can be accessed by the method \code{ctnames}.
#'  
#'  Then a named list of contracts can be easily created by the following:
#'  list(ctnames(c1)=c1, ctnames(c2)=c2)
#'  
#'  An instance of class \code{Portfolio} can be created by writing for example:
#'  p <- Portfolio(c1, xyz=c2)
#'  
#'  Then, typing 
#'  p
#'  should display the names of the contracts contained in the portfolio
#'  A single contract can be accessed by the \code{"[["} operator:
#'  p[["CT001"]]
#'  or
#'  p[[1]]
#'  
#'  A part of the portfolio can be extracted by using the \code{"["]} operator:
#'  p[1:2]
#'  
#'  A contract can be added to the portfolio by means of the \code{"[[<-"} operator:
#'  p[["CT003"]] <- c3
#'  Then, "CT003" is automatically set as \code{ContractID} of \code{c3}.
#'  Remark:
#'  The queston here is what should have priority, the name already contained in 
#'  the attribute "ContractID" or the name given in the brackets.
#'  I suggest that the name in the brackets should have priority.
#'  If you would like to use the existing ContractID, you can write:
#'  p[[ctnames(c3)]] <- c3
#'  or
#'  p <- c(p, c3)
#'  
#'  Two portfolios can be merged by the \code{c} method:
#'  p.both <- c(p1, p2)
#'  
