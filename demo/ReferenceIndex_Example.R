ind <- Index()
times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", "2018-01-01T00",
           "2019-01-01T00")
values <- c(100, 110, 120, 130, 140)
set(ind, what=list(MarketObjectCode = "CHF_SMI",
                   Data=list(Dates=times,Values=values)))
get(ind, "MarketObjectCode")
valueAt(ind, "2016-01-01T00")
valueAt(ind, c("2016-01-01T00", "2018-07-01T00", "2018-07-01T00"))