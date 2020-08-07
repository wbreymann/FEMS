#' @include YieldCurve2.R yearFraction.R
#' @export
setGeneric(name = "discountFactorsv2",
           def = function(object, termEnd, termStart, ...){
             standardGeneric("discountFactorsv2")
           })

#' @export
setMethod(f = "discountFactorsv2",
          signature = c("YieldCurve2", "character", "missing"),
          definition = function(object, termEnd, termStart, method = "continuous", period = "Y", ...) {
            termStart <- object$ReferenceDate
            return(discountFactorsv2(object, termEnd, termStart, method, period, ...))
          })

# To consider: Implementierung von Unterjährigen Zinsen bei Bruchteilen von Perioden
#' @export
setMethod(f = "discountFactorsv2",
          signature = c("YieldCurve2", "character", "character"),
          definition = function(object, termEnd, termStart,
                                method = "continuous", period = "Y", ...){
            
            # test dates and convert possible termEnd to date type
            test.dates(termStart)
            err_testing <- tryCatch(test.dates(termEnd), error = function(e) e)
            if (any(class(err_testing) == "error")) {
              endDate <- computeTenorDates(termStart, termEnd)
            } else {
              endDate <- termEnd
            }
            
            # calcluate year fraction
            yearFraction <- yearFraction(termStart, endDate, object$DayCountConvention)
            
            # get the required rate from the yield curve
            rates <- getRateAt(object, termStart, endDate, method = method, period = period)
            
            # return requested discount factors
            if (method == "linear") {
              return((1 + rates*abs(yearFraction))^sign(-yearFraction))
            } else if (method == "compound") {
              num_period <- convert.rate.period(period)
              return((1 + rates/num_period)^(-yearFraction*num_period))
            } else if (method == "continuous") {
              return(exp(-yearFraction * rates))
            } else {
              stop(paste("ErrorIn::discountFactorsv2:: Method ", method, " not supported !!!"))
            }
          })

