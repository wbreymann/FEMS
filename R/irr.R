##############################################################
#' \code{irr}
#'
#' Function computing the internal rate of return of a series of cash flows.
#' Computation of the internal rate of return requires the numerical
#' determination of the zeroes of a polynominal. In general these may be complex. 
#' A unique real solution is ensured if the first cash flow is negative and
#' all the following cash flows are non-negative with at least one of them 
#' being strictly positive, or the first cash flow is positive and all of the
#' followings are non-positive with at least one of them being strictly negative.
#' 
#' @param object a timeSeries object representing cash flows.
#' 
#' @param curve an object of type \code{\link{YieldCurve}} or 
#'              \code{\link{DynamicYieldCurve}} used for discounting the cash
#'              flows.
#' 
#' @param method (optional) character indicating the type of discounting. Can be
#'               one of "continuous", "compound" or "linear".
#' 
#' @param period (optional) character indicating periods of interest payments. 
#'               Can be "Y", "M", "W" or "D" for yearly, monthly, weekly or daily
#'               interest payments.
#' 
#' @return the internal rate of return calculated from the cash flows.
#' 
#' @usage irr(object, curve, method, period, ...)
#' 
#' @examples
#' yc_flat <- MarketInterestRate(0.05, "2015-01-01")
#' cfs <- timeSeries(data = c(5, 5, 105), charvec = c("2015-01-01","2016-01-01","2017-01-01"))
#' r <- irr(cfs, yc_flat)
#' 
#' @include BaseContract.R YieldCurve.R DynamicYieldCurve.R yearFraction.R Value.R
#' @export
setGeneric(name = "irr",
           def = function(object, curve, ...){
             standardGeneric("irr")
           })

#' @export
setMethod(f = "irr",
          signature = c("timeSeries", "YieldCurve"),
          definition = function(object, curve, ...) {
            return(irr(object,to.dynamic(curve), ...))
          })

#' @export
setMethod(f = "irr",
          signature = c("timeSeries", "DynamicYieldCurve"),
          definition = function(object, curve, method = "compound", period = "Y", ...) {

            # construct a BaseContract object, since value is already defined for it
            bc <- BaseContract(Dates = rownames(object),
                               CashFlows = as.numeric(object))
            
            # get net present value of the cash flows at the first date
            val <- value(bc, by = bc$Dates[1], curve = curve, compound = method, period = period)
            
            # extract dates and cash flows, adjsuted for initial NPV (for function definition)
            dts <- yearFraction(bc$Dates[1], bc$Dates, convention = curve$DayCountConvention)
            cfs <- bc$CashFlows
            cfs[1] <- cfs[1] - as.numeric(val)
            
            # define the function for which to find roots
            f <- function(r) {
              if (method == "linear") {
                sum(cfs * (1 + r*dts)^(-1))
              } else if (method == "compound") {
                m_period <- convert.rate.period(period)
                sum(cfs * (1 + r/m_period)^(-dts*m_period))
              } else if (method == "continuous") {
                sum(cfs * exp(-r*dts))
              } else {
                stop("Compounding method does not exist. Must be one of 'continuous', 'compound' or 'linear'!")
              }
            }
            
            # check if "interval" and "tol" is part of args and pass to uniroot
            args <- list(...)
            if ("interval" %in% args) {
              int <- args[["interval"]]
            } else {
              int <- c(-1, 1)
            }
            if ("tol" %in% args) {
              t <- args[["tol"]]
            } else {
              t <- 1e-9
            }
            
            # find root of function & return
            irr <- uniroot(f, interval = int, tol = t, ...)
            return(irr$root)
          })