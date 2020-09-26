##############################################################
#' Internal Rate of Return (\code{irr})
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
#' @param method character indicating the type of discounting. Can be
#'               one of "continuous", "compound" or "linear".
#' 
#' @param period character indicating periods of interest payments. 
#'               Can be "Y", "M", "W" or "D" for yearly, monthly, weekly or daily
#'               interest payments.
#' 
#' @param convention character indicating the day count convention.
#' 
#' @param ... additional arguments passed on to \code{\link{uniroot}}.
#'
#' @param isPercentage a logical, indicating if the results is returned as percentage 
#'                     (TRUE) or as fraction (FALSE) (default is TRUE). 
#'                     
#' @return The internal rate of return calculated from the cash flows.
#' 
#' @usage irr(object, method = "compound", period = "Y", convention = "30E360E", 
#'        isPercentage=TRUE, ...)
#' 
#' @examples
#' # timeSeries object
#' cfs <- timeSeries(data = c(-100, 5, 5, 105), 
#'                   charvec = c("2014-01-01","2015-01-01","2016-01-01","2017-01-01"))
#' r <- irr(cfs)
#' 
#' # Bond
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            coupon = 0.02, couponFreq = "1 years")
#' irr(b)
#' 
#' # Annuity
#' ann <- annuity("2013-12-31", nominal = 50000, ir = 0.02, maturity = "5 years")
#' irr(ann)
#'           
#' @export
setGeneric(name = "irr",
           def = function(object, ...){
             standardGeneric("irr")
           })

#' @export
setMethod(f = "irr",
          signature = c("ContractType"),
          definition = function(object, method = "compound", period = "Y", 
                                convention = "30E360", isPercentage=TRUE, ...) {
            browser()
            if (!any(is(object) %in% c("PrincipalAtMaturity","Annuity"))) {
              stop("irr currently only defined for bonds and annuities!")
            }
            cfs <- cashFlows(object)
            return(irr(cfs, method = method, period = period, convention = convention, 
                       isPercentage=isPercentage, ...))
          })

#' @export
setMethod(f = "irr",
          signature = c("timeSeries"),
          definition = function(object, method = "compound", period = "Y", 
                                convention = "30E360", isPercentage=TRUE, ...) {

            cfs <- as.numeric(object)
            dts <- yearFraction(rownames(object)[1], rownames(object), convention = convention)
            
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
            irr <- uniroot(f, interval = int, tol = t, ...)$root
            if (isPercentage) irr <- 100*irr
            return(irr)
          })