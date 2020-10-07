rm(list=ls())
devtools::load_all()

b1 <- bond(start = "2020-01-01", maturity = "5 years", nominal = 10000, 
           coupon = 0.05, ContractID="001")
a1 <- annuity("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
              , ContractID="002")
l1 <- loan("2020-01-01", nominal = 10000, ir = 0.05, maturity = "5 years",
           , ContractID="003")

ctnames(b1) # ok
as.name(ctnames(b1))

p1 <- Portfolio(b1, a1) # ok
ctnames(p1)
p1
ll <- list(b01=b1, a01=a1)
sum(names(ll) == "")

p1 <- Portfolio(b01=b1, a01=a1) # Error: Doesn't work. Now ok!!
p1
ctnames(p1)
class(p1)

ll <- list(b01=b1, a02=a1)
names(ll)
p2 <- Portfolio(ll) # ok
length(p2) # ok
ctnames(p2) # ok
names(p2$contracts)
p2  # error: no suitable show function defined
summary(p2) # ok

p2[[1]]$ContractTerms$ContractID  # ok
all.equal(p2[["b01"]], p2[[1]]) # ok

p2p1 <- c(p2, l1) # ok
class(p2p1)
length(p2p1)
ctnames(p2p1)
p2p1[["a02"]]

p0 <- Portfolio(list(l01=l1))
length(p0)
pc <- c(p1,p0) # error: contains only 1 contract instead of 3.
length(pc)
ctnames(pc)


p3 <- Portfolio(list(b01=b1, a02=a1, l01=l1))
length(p3)
length(p3[1:2])
all.equal(p3[1:2],p3[c("b01","a02")])  # OK

# Replacement operator
p3 <- Portfolio(list(b01=b1, a02=a1))
ctnames(p3)
p3[["l03"]] <- l1
ctnames(p3)
p3[[3]]$ContractTerms$ContractID  # ok
l1$ContractTerms$ContractID
ctnames(p3)[3] <- "l04"  # Error: Replacement doesn't work
p3[[3]] <- NULL  # Error: doesn't work. No way to delete a contract.

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


ll <- list(b01=b1, inv=invest)
p.mixed <- Portfolio(ll)  
length(p.mixed)
ctnames(p.mixed) # ok
p.mixed[["inv"]] # error: no access
p.mixed[[2]] # ok
p.mixed[["b01"]] # ok
all.equal(p.mixed[[1]], p.mixed[["b01"]])
p.mixed # Doesn't work
summary(p.mixed) # ok
