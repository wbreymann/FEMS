################################################################################
# Beginn des Lösungsskripts
################################################################################

library("devtools")
rm(list=ls())
devtools::load_all()


options(warn=-1)
# load required libraries
# library(rActus)
# library(rflPortfolio)
# library(rflSimulation)

# define analysis time
ad="2016-01-01T00"
(times = timeSequence(from=timeDate(substring(ad, 1, 10)), by="1 months", 
                      length.out=24))
# First, the market environment must be defined
# etc.
values = rnorm(24)
idx <- Index(
  MarketObjectCode = "GAS",
  Data = list(Dates=paste0(times,"T00"), Values=values))
plot(idx)
gas.ts = timeSeries(data=values, charvec=times)
plot(gas.ts)
# 
# This defines the yield curve observed at the analysis date. 
yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc.ch <- YieldCurve(MarketObjectCode = "YC_CH_EIDGENOSSEN", ReferenceDate = ad, 
                    Tenors = yc.tnr, Rates = yc.rts)
plot(yc.ch)
rf1 = RFConn(list(yc.ch, idx))
rf1


# Define Operations contract for operational cash flow pattern
# Ausgaben bzw. einnahmen für Rohstoff
ops.profit = function(model, params) { # rename to ops-profit
  timeSeries( valueAt(get(model, "GAS"), paste0(times, "T00")) * 1000, times)
}
plot(ops.profit(rf1, "GAS"))

#-----------------------------------------------------------------------------
# Modelling of operational revenues and expenses (Betriebskosten und -erträge)
# create Operations contract with "CashFlowPattern"
ops1 = Operations(ContractID="Ops001",
           Currency="CHF",
           CashFlowPattern = ops.profit,
           CashFlowParams = list(model=rf1, params="GAS"))

terms(ops1)
ops1$ContractType
ops1$CashFlowPattern
ops1$CashFlowParams

# link Operations contract with market environment
set(ops1, rf1)
# pure events
ad
events1 = events(ops1, ad)
# print(events1)  # Error
events1
# Evaluation of events starts 1 year later:
events1a = events(ops1, "2017-01-01")
# print(events1)  # Error
events1a

# liquidity
by <- times[c(1, 13, 24)]
tb <- timeBuckets(by, bucketLabs=c("2016", "2017"))
tb
liquidity(ops1, by=tb, type="marginal")
liquidity(ops1, by=tb, type="marginal", digits=0)

# Test von Hand:
1000*sum(values[2:13])
1000*sum(values[14:24])

# income
income(ops1, by=tb, type="marginal")  ## Error: methods still commented out.

# nominal value
value(ops1, by=by, type="nominal")

# Discount-Engine für die Barwertberechnung:
eng <- DcEngine(RiskFactorObjectLink="YC_CH_EIDGENOSSEN")
set(eng, rf1)

# Barwert der (erwarteten) operativen Cashflows
value(ops1, by=tb, type="markToModel", method=eng) ## Error

#-----------------------------------------------------------------------------
# Investition mit linearer Abschreibung
ops.invest <- function(model, params) {
  timeSeries(seq(1000, 0, length.out=24), times)
}

ops2 <- Operations(ContractID = "Ops002", Currency = "CHF", InvestPattern = ops.invest)

# link Operations contract with market environment
set(ops2, rf1)

# Test der Funktion
ops.invest(rf1)

# Analyse

# pure events
events2 = events(ops2, ad)
print(events2)   ## Error: must be adapted
events2

# Liquidität
liquidity(ops2, by=tb, type="marginal")

# Income
income(ops2, by=tb, type="marginal")  ## Error

# nominal value
value(ops2, by=by, type="nominal")
value(ops2, by=by, type="nominal", digits=0)

# mark-to-model value
# An der Grenze von 2 Timebuckets überprüfen.
# Wahrscheinlich Fehler im Package
value(ops2, by=tb, type="markToModel", method=eng)  # Insterestingly, this one is working now.

# Reserve contract
ops.reserve <- function(model,params) {
  timeSeries(seq(0, 1000, length.out=24), times)
}

ops3 <- Operations(ContractID="Ops003", Currency="CHF", ReservePattern=ops.reserve)

# link Operations contract with market environment
set(ops3, rf1)

# Test der Funktion
ops.reserve(rf1)

# analysis

# pure events
events3 <- events(ops3, ad)
print(events3)  # Error
events3

# liquidity
liquidity(ops3, by= tb, type = "marginal")

# income
income(ops3, by=tb, type="marginal")

# nominal value
value(ops3, by=by, type="nominal")

# mark-to-model value
value(ops3, by=tb, type="markToModel", method=eng)  ## Is also working now.

