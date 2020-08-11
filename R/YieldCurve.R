#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************


##############################################################
#' A Reference Class extending \code{\link{RiskFactor}} class
#' and representing a yield curve risk factor
#' 
#' Yield curves define a class of market risk factors 
#' that in the ACTUS model may directly affect future cash 
#' flows arising from a financial instrument, e.g. Floating-
#' rate bonds, or are used for valuation of instruments, 
#' e.g. in discounting.
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{RiskFactor, ReferenceIndex, ForeignExchangeRate}}
#'
#' @examples
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' 
#' get(ind, "MarketObjectCode")
#' get(yc, "ReferenceDate")
#' get(yc, "Tenors")
#' get(yc, "Rates")
#' 
#' @include RiskFactor.R
#' @export
#' @rdname ind-classes
setRefClass("YieldCurve", 
            contains = "RiskFactor",
            fields = list(ReferenceDate = "character",
                          Tenors = "character",
                          Rates = "numeric",
                          TenorDates = "character",
                          DayCountConvention = "character"))
                          


##############################################################
#' \code{YieldCurve}-class constructor
#'
#' Create an instance of \code{YieldCurve} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM.
#' 
#' @param ...
#'
#' @return An object of class \code{YieldCurve} 
#'          containing the reference to the Java object
#' 
#' @seealso \code{\link{ReferenceIndex, ForeignExchangeRate}}
#'
## @include
#' @export
#' @docType methods
#' @rdname yc-methods
#' @aliases YieldCurve-method
setGeneric(name = "YieldCurve",
           def = function(...){
             standardGeneric("YieldCurve")
           })

## @include
#' @export
#' @rdname yc-methods
## @aliases 
setMethod(f = "YieldCurve",signature = c(),
          definition = function(...){
            
            pars <- list(...)
            
            # if pars is a data.frame, convert to list()
            
            # fill fields with NULL values
            fill_fields <- list()
            fill_fields$MarketObjectCode <- "Generic_Yield_Curve"
            fill_fields$ReferenceDate <- as.character(today())
            fill_fields$Tenors <- "0M"
            fill_fields$Rates <- Inf
            fill_fields$DayCountConvention <- "30E/360"
            
            if (length(names(pars)) != 0) {
              
              # check if some fields are incorrectly specified!
              all_fields <- names(getRefClass("YieldCurve")$fields())
              pars_names <- names(pars)
              test_pars_names <- pars_names %in% all_fields
              if (!all(test_pars_names)) {
                stop(paste("ErrorInYieldCurve:: Yield Curve has no field called: ", 
                           pars_names[!test_pars_names], "!!!"))
              }
              
              # check if necessary fields are missing and set the provide field names
              if (length(names(pars)) > 1) {
                if (!all(c("ReferenceDate","Tenors","Rates") %in% names(pars))) {
                  stop(paste(
                    "ErrorInYieldCurve:: If any, all of the following fields have to be provided: "
                    , paste(c("ReferenceDate","Tenors","Rates"),collapse = ", "), "!!!"))
                } else {
                  fill_fields$ReferenceDate <- pars$ReferenceDate
                  fill_fields$Tenors <- pars$Tenors
                  fill_fields$Rates <- pars$Rates
                }
              }
              
              # use if MarketObjectCode is provided,
              if ("MarketObjectCode" %in% pars_names) {
                fill_fields$MarketObjectCode <- pars$MarketObjectCode
              }
              if ("DayCountConvention" %in% pars_names) {
                fill_fields$DayCountConvention <- pars$DayCountConvention
              }
            }

            yc <- new("YieldCurve")
            for (nms in names(fill_fields)) {
                yc[[nms]] <- fill_fields[[nms]]
            }

            test.dates(yc$ReferenceDate)
            yc$TenorDates <- computeTenorDates(yc$ReferenceDate, yc$Tenors)
            return(yc)
            })

#' @export
setGeneric(name = "FlatCurve",
           def = function(rate, ref_date){
             standardGeneric("FlatCurve")
           })

#' @export
setMethod(f = "FlatCurve", signature = c("numeric","character"),
          definition = function(rate, ref_date){
            yc <- YieldCurve()
            tenors <- c("1W", "6M", "1Y", "5Y", "10Y", "50Y", "100Y")
            rates <- rep(1, length(tenors)) * rate
            set(yc, what = list(
              MarketObjectCode = "YC_Flat",
              ReferenceDate = ref_date,
              Tenors = tenors,
              Rates = rates))
            return(yc)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ReferenceIndex,list-method
setMethod(f = "set", signature = c("YieldCurve", "list"),
          definition = function(object, what, ...){

            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.yieldcurve.field(i)) {
                value <- what[[i]]
                switch(i,
                       ReferenceDate = {
                         object$ReferenceDate <- value
                       },
                       Rates = {
                         object$Rates <- value
                       },
                       Tenors = {
                         object$Tenors <- value
                         object$TenorDates <- computeTenorDates(yc$ReferenceDate, value)
                       },
                       DayCountConvention = {
                         object$DayCountConvention <- tolower(value)
                       },
                       MarketObjectCode = {
                         object$MarketObjectCode <- value
                       }
                )
              } else {
                warning(paste("ErrorInYieldCurve:: Field ", i, " does not exist, cannot assign value!", sep = ""))
              }
            }
            if (length(object$Tenors) != length(object$Rates)) {
              stop("ErrorInYieldCurve::set:: Rates must have same length as Tenors")
            }
          })


## @include
#' @export
#' @rdname get-methods
#' @aliases get,RiskFactorConnector,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ReferenceIndex,character-method
setMethod(f = "get", signature = c("YieldCurve", "character"),
          definition = function(object, what, ...){
            out <- list()
            if (length(what) == 1 && tolower(what) == "all") {
              what <- names(object$getRefClass()$fields())
            }
            for (i in what) {
              if (is.valid.yieldcurve.field(i)) {
                out[[i]] <- switch(i,
                                   MarketObjectCode = object$MarketObjectCode,
                                   ReferenceDate = object$ReferenceDate,
                                   Tenors = object$Tenors,
                                   Rates = object$Rates,
                                   DayCountConvention = object$DayCountConvention,
                                   TenorDates = object$TenorDates
                )
              } else {
                warning(paste("ErrorInYieldCurve::get:: Field ", i, " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })

#########################################################################################
#' Computes the forward rate from time t1 to time t2.
#' 
#' Q bwlf:
#' Is this a helper method or do we need a help text?
#' A auth:
#' This function still needs documentation, yes.
#' 
#' @export
setGeneric(name = "getRateAt",
           def = function(object, from, to){
             standardGeneric("getRateAt")
           })

#########################################################################################
#' @export
setMethod(f = "getRateAt",
          signature = c("YieldCurve", "character", "character"),
          definition = function(object, from, to){
            
            if (as.Date(from)<object$ReferenceDate || as.Date(to)<object$ReferenceDate) {
              stop("ErrorIn::YieldCurve::getRateAt:: No Yields can be calculated before ReferenceDate of the YieldCurve!!!")
            }
            
            # set the interpolator with year fractions and rates
            t1 <- yearFraction(object$ReferenceDate, from, object$DayCountConvention)
            t2 <- yearFraction(object$ReferenceDate, to, object$DayCountConvention)
            
            interpolator <- Interpolator(xValues = yearFraction(object$ReferenceDate, 
                                                                object$TenorDates, 
                                                                object$DayCountConvention), 
                                         yValues = object$Rates)
            
            # get rates from interpolation
            s1 <- interpolator$getValueAt(t1)
            s2 <- interpolator$getValueAt(t2)
            
            # calculate forward rate
            f12 <- (t2*s2 - t1*s1)/(t2 - t1)
            f12[is.na(f12)] <- 0
            return(f12)
          })


#' @export
setGeneric(name = "setTimeSeries",
           def = function(object, startdate, enddate, ...){
             standardGeneric("setTimeSeries")
           })

# Q bwlf:
# Wo wird diese Methode gebraucht?
# A auth:
# wird gebraucht um Zeitreihe von rates zu berechnen, welche an die 
# API geschickt wird. (siehe auch YieldCurve_Example.R)
 


#' @export
setMethod(f = "setTimeSeries",
          signature = c("YieldCurve", "character", "character"),
          definition = function(
            object, startdate, enddate, frequency = "month", forward = "1M", ...){
            object$Data <- getRateSeries(object, startdate, enddate, 
                                               frequency = frequency, forward = forward)
          })

##############################################################
#' Generic method to retrieve the rate(s) for a specific
#' tenor(s) from a \code{\link{YieldCurve}} object
#'
#' A yield curve is a time-structure of yields, i.e. for
#' different future points in time (tenors) a yield is 
#' extracted from observed instrument prices. The 
#' \code{\link{YieldCurve}} object contains these tenors with
#' associated yields and allows to retrieve yields for any
#' tenor by inter-/extrapolation.
#' 
#' @param object An object of class \code{YieldCurve} for 
#'        which to return the yield for a given tenor
#'        
#' @param termEnd The tenor for which to return its yield. 
#'        Can be a single value or a vector of tenors.
#' 
#' @param termStart (optional) For the forward rate at t0 
#'        between times t1 and t2, termEnd refers to t2 and
#'        termStart to t1. Is a single value. If combined 
#'        with a vector for termEnd, then termStart remains
#'        the same for all t2 defined in termEnd.
#' 
#' @param ... Additional parameters:
#' \itemize{
#'  \item{"isDateEnd"}{logical indicating whether termEnd is 
#'        of date (TRUE) or term (FALSE) format. Date format is
#'        'YYYY-MM-DDTXX' with 'XX'=00 for beginning of day, or
#'        24 for end of day, and term format is 'IP' with 'I' 
#'        an integer and 'P' indicating the period (D=days, 
#'        W=weeks, M=months, Q=quarters, H=halfyears, Y=years).
#'        Default is isDateEnd=FALSE.}
#' }
#'
#' @return numeric The yield for the defined tenor
#' 
#' @seealso \code{\link{discountFactors}}
#' 
#' @examples
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' 
#' rates(yc, "1Y")  # 1-year spot rate
#' rates(yc, "2016-01-01T00", isDateEnd=TRUE) # again, 1-year spot rate
#' rates(yc, "1Y", "2015-07-01T00")  # 1-year forward rate at 2015-07-01T00
#'
## @include
#' @export
#' @docType methods
#' @rdname rts-methods
#' @aliases rates, YieldCurve, charachter, missing-method
#' @aliases rates, YieldCurve, character, character-method
setGeneric(name = "rates",
           def = function(object, termEnd, termStart, ...){
             standardGeneric("rates")
           })

## @include
#' @export
#' @rdname rts-methods
#' @aliases rates, YieldCurve, character, missing-method
setMethod(f = "rates",
          signature = c("YieldCurve", "character", "missing"),
          definition = function(object, termEnd, termStart, isDateEnd = FALSE, ...){
            if (!isDateEnd) {
              # endDate <- computeTenorDates(object$ReferenceDate, termEnd)
              endDate <- shiftDates(object$ReferenceDate, termEnd)
            } else {
              endDate <- termEnd
            }
            test.dates(endDate)
            out <- getRateAt(object, object$ReferenceDate, endDate)
            return(out)
            
          })

## @include
#' @export
#' @rdname rts-methods
#' @aliases rates, YieldCurve, character, character-method
setMethod(f = "rates",
          signature = c("YieldCurve", "character", "character"),
          definition = function(object, termEnd, termStart, isDateEnd = FALSE, ...){

            if (!isDateEnd) {
              # endDate <- computeTenorDates(termStart, termEnd)
              endDate <- shiftDates(termStart, termEnd)
            } else {
              endDate <- termEnd
            }
            test.dates(termStart)
            test.dates(endDate)
            out <- getRateAt(object, termStart, endDate)
            return(out)
          })

##############################################################
#' Generic method to retrieve discount factors for specific
#' tenor(s) from a \code{\link{YieldCurve}} object
#'
#' Discount factors for \code{t2}-tenors are extracted from a 
#' \code{\link{YieldCurve}} object according to 
#' \code{df(t0,t1,t2)=exp(-yf(t1,t2)*yield(t1,t2))} where
#' \itemize{
#'  \item{"t0"}{is the 'ReferenceDate' of the yield curve}
#'  \item{"t1"}{marks the discounting period start. If t1>t0, 
#'              we are in a forward-mode (default: t0=t1, i.e. 
#'              non-forward)}
#'  \item{"t2"}{marks the discounting period end}
#'  \item{"yf(t1,t2)"}{indicates the the year fraction using 
#'                    Actual/Actual between times t1 and t2}
#'  \item{"yield(t1,t2)"}{refers to the yield from t1 to t2 
#'                    implied by the yield curve (forward 
#'                    rate/yield if t1>t0)}
#' }
#' 
#' @param object An object of class \code{YieldCurve} from 
#'        which to extract the discount factors
#'        
#' @param termEnd The discounting period end date (t2) 
#'        Can be a single value or a vector of tenors.
#' 
#' @param termStart (optional) The discounting period start
#'        date (t1). Is a single value. If combined 
#'        with a vector for termEnd, then termStart remains
#'        the same for all t2 defined in termEnd.
#' 
#' @param ... Additional parameters:
#' \itemize{
#'  \item{"isDateEnd"}{logical indicating whether termEnd is 
#'        of date (TRUE) or term (FALSE) format. Date format is
#'        'YYYY-MM-DDTXX' with 'XX'=00 for beginning of day, or
#'        24 for end of day, and term format is 'IP' with 'I' 
#'        an integer and 'P' indicating the period (D=days, 
#'        W=weeks, M=months, Q=quarters, H=halfyears, Y=years).
#'        Default is isDateEnd=FALSE.}
#' }
#' 
#' @return numeric The discount factor for the defined period
#' 
#' @seealso \code{\link{rates}}
#' 
#' @examples
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(MarketObjectCode = "YC_Prim",
#'   Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))
#' 
#' discountFactors(yc, "1Y")
#' discountFactors(yc, "2016-01-01T00", isDateEnd=TRUE)
#' discountFactors(yc, "1Y", "2015-07-01T00")
#'
## @include
#' @export
#' @docType methods
#' @rdname dfs-methods
## @aliases
setGeneric(name = "discountFactors",
           def = function(object, termEnd, termStart, ...){
             standardGeneric("discountFactors")
           })

## @include
#' @export
#' @rdname dfs-methods
#' @aliases discountFactors, YieldCurve, character, character-method
setMethod(f = "discountFactors",
          signature = c("YieldCurve", "character", "missing"),
          definition = function(object, termEnd, termStart, isDateEnd = FALSE, spread = 0.0, ...) {
            if (!isDateEnd) {
              endDate <- computeTenorDates(object$ReferenceDate, termEnd)
              # endDate <- shiftDates(object$ReferenceDate, termEnd)
            } else {
              endDate <- termEnd
            }
            yearFraction <- yearFraction(object$ReferenceDate, endDate, object$DayCountConvention)
            test.dates(endDate)
            rate <- getRateAt(object, object$ReferenceDate, endDate)
            return(exp(-yearFraction * rate))
          })

## @include
#' @export
#' @rdname dfs-methods
#' @aliases discountFactors, YieldCurve, character, missing-method
setMethod(f = "discountFactors",
          signature = c("YieldCurve", "character", "character"),
          definition = function(object, termEnd, termStart,
                                isDateEnd = FALSE, spread = 0.0, ...){
            if (!isDateEnd) {
              endDate <- computeTenorDates(termStart, termEnd)
              # endDate <- shiftDates(termStart, termEnd)
            } else {
              endDate <- termEnd
            }
            yearFraction <- yearFraction(termStart, endDate, object$DayCountConvention)
            test.dates(termStart)
            test.dates(endDate)
            rate <- getRateAt(object, termStart, endDate)
            return(exp(-yearFraction * rate))
          })


#' @export
setGeneric(name = "getRateSeries",
           def = function(object, startdate, enddate, ...){
             standardGeneric("getRateSeries")
           })
#' @export
setMethod(f = "getRateSeries",
          signature = c("YieldCurve", "character", "character"),
          definition = function(
            object, startdate, enddate, frequency = "week", forward = "1M", ...){
            
            # check freq inputs are valid
            test.dates(startdate)
            test.dates(enddate)
            if (!frequency %in% c("day", "week", "month", "quarter", "year")) {
              stop("ErrorIn::YieldCurve::getRateSeries:: Frequency must be one of 'day',
                   'week', 'month', 'quarter' or 'year' !!!")
            }
            
            # create date vector with frequency defined
            dt_seq <- as.character(seq.Date(as.Date(startdate) + 1, 
                                            as.Date(enddate), by = frequency) - 1)
            
            # calculate forward rates with forward time defined
            rates_seq <- rates(object, forward, dt_seq)
            
            # output in a data.frame
            ts <- data.frame(Dates = dt_seq,
                          Values = as.numeric(rates_seq),
                          stringsAsFactors = FALSE)
            return(ts)
          })


## @include
#' @export
setMethod(f = "show", signature = c("YieldCurve"),
          definition = function(object){
            cat(paste0("MarketObjectCode: ", object$MarketObjectCode,"\n"))
            cat(paste0("ReferenceDate: ", object$ReferenceDate,"\n"))
            cat(paste0("DayCountConvention: ", object$DayCountConvention,"\n"))
            curve <- object$Rates
            names(curve) <- object$Tenors
            print("Curve:")
            print(curve)
          })

## @include
#' @export
setMethod(f = "names", signature = c("YieldCurve"),
          definition = function(x){
            return(names(x$getRefClass()$fields()))
          })


# WHAT are these two for???
## @include
## @export
# setMethod("[[", signature = c("YieldCurve", "ANY"),
#           definition = function(x, i) {
#             l <- x
#             names(l[["Rates"]]) = l[["Tenors"]]
#             l[[i]]
#           }
# )

## @include
# @export
# setMethod("[[<-", signature = c("YieldCurve", "ANY"),
#           definition = function(x, i, value) {
#             y <- x
#             y[[i]] <- value
#             set(x, list(
#               MarketObjectCode = y[["MarketObjectCode"]],
#               ReferenceDate = y[["ReferenceDate"]],
#               Tenors = y[["Tenors"]],
#               Rates = y[["Rates"]],
#               DayCountConvention = y[["DayCountConvention"]],
#               TenorDates = y[["TenorDates"]]
#             ))
#             x
#           }
# )




##############################################################
#' A Reference Class 
#' 
Interpolator <- setRefClass("Interpolator", 
            fields = list(xValues = "numeric",
                          yValues = "numeric"),
            methods = list(
              initialize = function(...) {
                pars <- list(...)
                
                all_fields <- names(getRefClass("Interpolator")$fields())
                pars_names <- names(pars)
                test_pars_names <- pars_names %in% all_fields
                if (!all(test_pars_names)) {
                  stop(paste(
                    "ErrorInYieldCurve::Interpolator:: Interpolator has no field called: ", 
                    pars_names[!test_pars_names], "!!!"))
                }
                
                if (length(pars$xValues) != length(pars$yValues)) {
                  stop("ErrorInYieldCurve::Interpolator:: xValues and yValues must have same length !!!")
                }
                .self$xValues <- pars$xValues
                .self$yValues <- pars$yValues
              },
              getValueAt = function(x){
                xOut <- approx(xValues, yValues, x, method = "linear", rule = 2)
                return(xOut$y)
              }
            ))


## -----------------------------------------------------------------
## helper methods
# existing fields in the YieldCurve class
validYieldCurveFields <- function() {
  return(c("Rates", "Tenors", "ReferenceDate", "MarketObjectCode", 
           "DayCountConvention", "TenorDates"))
}

# check if fields are valid
is.valid.yieldcurve.field <- function(x) {
  valid <- validYieldCurveFields()
  return(x %in% valid)
}

# convert character terms to dates relative to a refDate
computeTenorDates <- function(refDate, tenors){
  
  relativeDates <- c("")
  for (i in 1:length(tenors)) {
    count <- as.numeric(substr(tenors[i], 1, nchar(tenors[i])-1))
    switch(substr(tenors[i], nchar(tenors[i]), nchar(tenors[i])),
           "D" = {
             relativeDates[i] <- as.character(ymd(refDate) %m+% days(count))
           },
           "W" =  {
             relativeDates[i] <- as.character(ymd(refDate) %m+% weeks(count))
           },
           "M" = {
             relativeDates[i] <- as.character(ymd(refDate) %m+% months(count))
           },
           "Q" = {
             quarter_count <- count * 3
             relativeDates[i] <- as.character(ymd(refDate) %m+% months(quarter_count))
           },
           "H" = {
             halfyear_count <- count * 6
             relativeDates[i] <- as.character(ymd(refDate) %m+% months(halfyear_count))
           },
           "Y" = {
             relativeDates[i] <- as.character(ymd(refDate) %m+% years(count))
           }
    )
  }
  return(relativeDates)
}

shiftDates <- function(dates, shift){
  
  count <- as.numeric(substr(shift, 1, 1))
  switch(substr(shift, nchar(shift), nchar(shift)),
         "D" = {
           relativeDates <- as.character(ymd(dates) %m+% days(count))
         },
         "W" =  {
           relativeDates <- as.character(ymd(dates) %m+% weeks(count))
         },
         "M" = {
           relativeDates <- as.character(ymd(dates) %m+% months(count))
         },
         "Q" = {
           quarter_count <- count * 3
           relativeDates <- as.character(ymd(dates) %m+% months(quarter_count))
         },
         "H" = {
           halfyear_count <- count * 6
           relativeDates <- as.character(ymd(dates) %m+% months(halfyear_count))
         },
         "Y" = {
           relativeDates <- as.character(ymd(dates) %m+% years(count))
         }
  )
  return(relativeDates)
}


test.dates <- function(date) {
  tryCatch({
    as.Date(date)
  }, error = function(e) {
    stop("ErrorIn::YieldCurve Dates are not valid !!!")
  })
}


convert.rate.period <- function(period) {
  allowed_periods <- c(1, 2, 4, 12, 52, 360)
  names(allowed_periods) <- c("Y", "H", "Q", "M", "W","D")
  
  if(period %in% names(allowed_periods)) {
    period_num <- allowed_periods[[period]]
  } else {
    stop(paste("ErrorIn::discountFactorsv2:: ", period, " is not a valid interest rate period !!!", sep=" "))
  }
  return(period_num)
}
