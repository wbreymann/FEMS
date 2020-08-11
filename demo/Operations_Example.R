################################################################################
# Beginn des Lösungsskripts
################################################################################

rm(list=ls())
devtools::load_all()

# define analysis time
ad="2016-01-01"
(times = timeSequence(from=timeDate(substring(ad, 1, 10)), by="1 months", 
                      length.out=24))
# Ausgaben bzw. einnahmen für Rohstoff
ops.profit = function(model, params) { # rename to ops-profit
  timeSeries( valueAt(get(model, "GAS"), paste0(times, "T00")) * 1000, times)
}
#-----------------------------------------------------------------------------
# Modelling of operational revenues and expenses (Betriebskosten und -erträge)
# create Operations contract with "CashFlowPattern"
ops1 = Ops(ContractID="Ops001",
           Currency="CHF",
           CashFlowPattern = ops.profit)

terms(ops1)
ops1$ContractType
ops1$CashFlowPattern
# etc.
values = rnorm(24)
idx <- Index(MarketObjectCode = "GAS",
             Data = list(Dates=times, Values=values))
plot(idx)
gas.ts = timeSeries(data=values, charvec=times)
plot(gas.ts)
# 
# This defines the yield curve observed at the analysis date. 
yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc.ch <- YieldCurve(MarketObjectCode = "YC_CH", ReferenceDate = ad, 
                    Tenors = yc.tnr, Rates = yc.rts)
plot(yc.ch)
rf = RFConn(list(yc.ch, idx))
rf
plot(ops.profit(rf, "GAS"))
# link Operations contract with market environment
set(ops1, rf)
# pure events
events1 = events(ops1, ad)
print(events1$evs)

# liquidity
by <- times[c(1, 13, 24)]
tb <- timeBuckets(by=by, bucketLabs=c("2016", "2017"))
tb
liquidity(ops1, by=tb, type="marginal")





# Test von Hand:
1000*sum(values[2:13])
1000*sum(values[14:24])

# income
#income(ops1, by=tb, type="marginal")

# nominal value
value(ops1, by=by, type="nominal")

# Discount-Engine für die Barwertberechnung:
eng <- DcEngine(RiskFactorObject=rf[["YC_CH"]])
#set(eng, rf)

# Barwert der (erwarteten) operativen Cashflows
value(ops1, by=tb, type="markToModel", method=eng)

#-----------------------------------------------------------------------------
# Investition mit linearer Abschreibung

tb <- timeBuckets(ad, by="1 years", length.out=3, bucketLabs=c("2016", "2017"))
tb
# Discount-Engine für die Barwertberechnung:
eng <- DcEngine(RiskFactorObject=yc.ch)

ops.invest <- function(model, params) {
  timeSeries(seq(1000, 0, length.out=24), times)
}

ops2 <- Ops(ContractID = "Ops002", Currency = "CHF", InvestPattern = ops.invest)

# link Operations contract with market environment
set(ops2, rf)

# Test der Funktion
ops.invest(rf)

# Analyse

# pure events
events2 = events(ops2, ad)
print(events2$evs)

# Liquidität
liquidity(ops2, by=tb, type="marginal")

# Income
#income(ops2, by=tb, type="marginal")

# nominal value
value(ops2, by=by, type="nominal")

# mark-to-model value
# An der Grenze von 2 Timebuckets überprüfen.
# Wahrscheinlich Fehler im Package
value(ops2, by=tb, type="markToModel", method=eng)
ops.reserve <- function(model,params) {
  timeSeries(seq(0, 1000, length.out=24), times)
}

