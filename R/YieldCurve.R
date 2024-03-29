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
                          DayCountConvention = "character",
                          FUN = "function"
                          # ,fParams = "list"
                          ))
                          


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
            fill_fields$label <- "Generic_Yield_Curve"
            fill_fields$ReferenceDate <- as.character(today())
            fill_fields$Tenors <- "0M"
            fill_fields$Rates <- Inf
            fill_fields$DayCountConvention <- "30E360"
            fill_fields$FUN <- NULL
            # names(fill_fields) <- 
            #   c("label", "ReferenceDate", "Tenors", "Rates", "DayCountConvention",
            #     "FUN")
            # fill_fields$fParams <- NULL
            
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
              
              # use if label is provided,
              if ("label" %in% pars_names) {
                fill_fields$label <- pars$label
              }
              if ("DayCountConvention" %in% pars_names) {
                fill_fields$DayCountConvention <- pars$DayCountConvention
              }
              if ("FUN" %in% pars_names) {
                fill_fields$FUN <- pars$FUN
                # if ( !("fParams" %in% pars_names) ) {
                #   stop(paste("If 'FUN' is defined, 'fParams' must be defined, too!"))
                # } 
                # fill_fields$fParams <- pars$fParams
              }
              
            }

            yc <- new("YieldCurve")
            for (nms in names(fill_fields)) {
                yc[[nms]] <- fill_fields[[nms]]
            }

            test.dates(yc$ReferenceDate)
            yc$TenorDates <- tenors2dates(yc$ReferenceDate, yc$Tenors)
            return(yc)
            })

##############################################################
#' \code{MarketInterestRate}
#'
#' Constructor for a \code{\link{YieldCurve}} object with constant
#' rates for all tenors.
#' 
#' @param rate a numeric to set the constant market rate.
#' 
#' @param date a character indicating the reference date.
#' 
#' @param label a character to name the \code{\link{YieldCurve}} object.
#'
#' @return an object of type \code{\link{YieldCurve}}. 
#' 
#' @usage MarketInterestRate(rate, refDate, label, ...)
#' 
#' @examples
#' yc_flat <- MarketInterestRate(0.05, "2015-01-01", "MarketRate")
#' 
#' @export
setGeneric(name = "MarketInterestRate",
           def = function(rate, date, ...){
             standardGeneric("MarketInterestRate")
           })

#' @export
setMethod(f = "MarketInterestRate", signature = c("numeric","character"),
          definition = function(rate, date, label = "MarketInterestRate", ...){
            yc <- YieldCurve()
            tenors <- c("1W", "6M", "1Y", "5Y", "10Y", "50Y", "100Y")
            rates <- rep(1, length(tenors)) * rate
            set(yc, list(label = label,
                        ReferenceDate = date,
                        Tenors = tenors,
                        Rates = rates))
            set(yc, list(...))
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
                         object$TenorDates <- tenors2dates(object$ReferenceDate, value)
                       },
                       DayCountConvention = {
                         object$DayCountConvention <- value
                       },
                       label = {
                         object$label <- value
                       }
                )
              } else {
                warning(paste("Error In Yield Curve:: Field ", i, 
                              " does not exist, cannot assign value!", sep = ""))
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
                                   label = object$label,
                                   ReferenceDate = object$ReferenceDate,
                                   Tenors = object$Tenors,
                                   Rates = object$Rates,
                                   DayCountConvention = object$DayCountConvention,
                                   TenorDates = object$TenorDates
                )
              } else {
                warning(paste("ErrorInYieldCurve::get:: Field ", i, 
                              " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })

## @include
#' @export
setMethod(f = "show", signature = c("YieldCurve"),
          definition = function(object){
            cat(paste0("Label: ", object$label,"\n"))
            cat(paste0("ReferenceDate: ", object$ReferenceDate,"\n"))
            cat(paste0("DayCountConvention: ", object$DayCountConvention,"\n"))
            if (length(unique(object$Rates))==1) {
              cat(paste0("MarketInterestRate: ", round(object$Rates[1]*100, 2),"%","\n"))
              cat("Constant for all tenors/terms.")
            } else {
              curve <- object$Rates
              names(curve) <- object$Tenors
              print("Curve:")
              print(curve)
            }
            if (!is.null(body(object$FUN))) {
              cat("\nFUN:\n")
              print(object$FUN)
              # cat("fParams:\n")
              # print(object$fParams)
            }
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
## @export

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




# ##############################################################
# #' A Reference Class 
# #' 
# #' 
# #' Note: A class of the same name is defined in 'DynamicYieldCurve.R'
# #' 
# #' @export
# Interpolator <- setRefClass("Interpolator", 
#             fields = list(xValues = "numeric",
#                           yValues = "numeric"),
#             methods = list(
#               initialize = function(...) {
#                 pars <- list(...)
#                 
#                 all_fields <- names(getRefClass("Interpolator")$fields())
#                 pars_names <- names(pars)
#                 test_pars_names <- pars_names %in% all_fields
#                 if (!all(test_pars_names)) {
#                   stop(paste(
#                     "ErrorInYieldCurve::Interpolator:: Interpolator has no field called: ", 
#                     pars_names[!test_pars_names], "!!!"))
#                 }
#                 
#                 if (length(pars$xValues) != length(pars$yValues)) {
#                   stop("ErrorInYieldCurve::Interpolator:: xValues and yValues must have same length !!!")
#                 }
#                 .self$xValues <- pars$xValues
#                 .self$yValues <- pars$yValues
#               },
#               getValueAt = function(x){
#                 xOut <- approx(xValues, yValues, x, method = "linear", rule = 2)
#                 return(xOut$y)
#               }
#             ))

## -----------------------------------------------------------------
## helper methods
# existing fields in the YieldCurve class
# ATTENTION: This is EXACT replica of a function in 'DynamicYieldCurve.R'
# This function is used nowhere in this package
# validYieldCurveFields <- function() {
#   return(c("Rates", "Tenors", "ReferenceDate", "label", 
#            "DayCountConvention", "TenorDates"))
# }

# check if fields are valid
# ATTENTION: This is EXACT replica of a function in 'DynamicYieldCurve.R'
# This function is used nowhere in this package
# is.valid.yieldcurve.field <- function(x) {
#   valid <- validYieldCurveFields()
#   return(x %in% valid)
# }

# convert character terms to dates relative to a refDate
# new function with extension in "DynamicYieldCurve.R
# tenors2dates <- function(refDate, tenors){
#   
#   relativeDates <- c("")
#   for (i in 1:length(tenors)) {
#     count <- as.numeric(substr(tenors[i], 1, nchar(tenors[i])-1))
#     switch(substr(tenors[i], nchar(tenors[i]), nchar(tenors[i])),
#            "D" = {
#              relativeDates[i] <- as.character(ymd(refDate) %m+% days(count))
#            },
#            "W" =  {
#              relativeDates[i] <- as.character(ymd(refDate) %m+% weeks(count))
#            },
#            "M" = {
#              relativeDates[i] <- as.character(ymd(refDate) %m+% months(count))
#            },
#            "Q" = {
#              quarter_count <- count * 3
#              relativeDates[i] <- as.character(ymd(refDate) %m+% months(quarter_count))
#            },
#            "H" = {
#              halfyear_count <- count * 6
#              relativeDates[i] <- as.character(ymd(refDate) %m+% months(halfyear_count))
#            },
#            "Y" = {
#              relativeDates[i] <- as.character(ymd(refDate) %m+% years(count))
#            }
#     )
#   }
#   return(relativeDates)
# }

# Duplicate, cf. DynamicYieldCurve.R
# shiftDates <- function(dates, shift){
#   
#   count <- as.numeric(substr(shift, 1, 1))
#   switch(substr(shift, nchar(shift), nchar(shift)),
#          "D" = {
#            relativeDates <- as.character(ymd(dates) %m+% days(count))
#          },
#          "W" =  {
#            relativeDates <- as.character(ymd(dates) %m+% weeks(count))
#          },
#          "M" = {
#            relativeDates <- as.character(ymd(dates) %m+% months(count))
#          },
#          "Q" = {
#            quarter_count <- count * 3
#            relativeDates <- as.character(ymd(dates) %m+% months(quarter_count))
#          },
#          "H" = {
#            halfyear_count <- count * 6
#            relativeDates <- as.character(ymd(dates) %m+% months(halfyear_count))
#          },
#          "Y" = {
#            relativeDates <- as.character(ymd(dates) %m+% years(count))
#          }
#   )
#   return(relativeDates)
# }

# Duplicate, cf. DynamicYieldCurve.R
# test.dates <- function(date) {
#   tryCatch({
#     as.Date(date)
#   }, error = function(e) {
#     stop("ErrorIn::YieldCurve Dates are not valid !!!")
#   })
# }


# Duplicate, cf. DynamicYieldCurve.R
# convert.rate.period <- function(period) {
#   allowed_periods <- c(1, 2, 4, 12, 52, 360)
#   names(allowed_periods) <- c("Y", "H", "Q", "M", "W","D")
#   
#   if(period %in% names(allowed_periods)) {
#     period_num <- allowed_periods[[period]]
#   } else {
#     stop(paste("ErrorIn::discountFactorsv2:: ", period, " is not a valid interest rate period !!!", sep=" "))
#   }
#   return(period_num)
# }
