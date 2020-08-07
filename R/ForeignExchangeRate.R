#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class extending \code{\link{RiskFactor}} class
#' and representing a foreign exchange rate risk factor
#' 
#' Foreign exchange rates define a class of market risk factors 
#' that in the ACTUS model may directly affect future cash 
#' flows arising from a financial instrument, e.g. Cash-settled
#' cross-currency-swaps, or are used to normalize cash flows in
#' different currencies for reporting.
#' 
#' @field MarketObjectCode character name of the risk factor 
#' @field CurrencyPair character currency pair represented 
#' by risk factor
#' @field TimeSeries data.frame representating time series data
#' 
#' @seealso \code{\link{RiskFactor, YieldCurve, ReferenceIndex}}
#'
#' @examples
#' 
#' # create an FX-Rate object
#' fx <- FxRate() 
#' 
#' # define time stamps and values
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
#'            "2018-01-01", "2019-01-01")
#' values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
#' 
#' # set the MarketObjectCode and TimeSeries
#' set(fx, what = list(MarketObjectCode = "CHF/USD",
#'                     TimeSeries = list(Dates = times, 
#'                                       Values = values)))  
#' 
#' # get MarketObjectCode
#' get(fx, "MarketObjectCode")
#' 
#' @include RiskFactor.R
#' @export
#' @rdname fx-classes
#' 
setRefClass("ForeignExchangeRate", contains = "RiskFactor",
            fields = list(CurrencyPair = "character"))

##############################################################
#' \code{ForeignExchangeRate}-class constructor
#'
#' Create an instance of \code{ForeignExchangeRate} class. 
#' 
#' @param ...
#'
#' @return An object of class \code{ForeignExchangeRate} 
#' 
#' @seealso \code{\link{YieldCurve, Index}}
#'
## @include
#' @export
#' @docType methods
#' @rdname fx-methods
#' @aliases FxRate-method
setGeneric(name = "FxRate",
           def = function(...){
             standardGeneric("FxRate")
           })

## @include
#' @export
#' @rdname fx-methods
## @aliases 
setMethod(f = "FxRate",signature = c(),
          definition = function(...){
            object = new("ForeignExchangeRate")
            pars = list(...)
            if (length(pars) != 0) {
              if (is.list(pars[[1]])) {
                set(object, what = pars[[1]])
              } else {
                set(object, what = pars)
              }
            }
            return(object)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,YieldCurve,list-method
setMethod(f = "set", signature = c("ForeignExchangeRate", "list"),
          definition = function(object, what, ...){
            par.names <- names(what)
            for (i in par.names) {
              if (FEMS:::is.valid.fxrate.set.field(i)) {
                value <- what[[i]]
                switch(i,
                       MarketObjectCode = {
                         object$MarketObjectCode = value
                       },
                       CurrencyPair = {
                         object$CurrencyPair = value
                       },                     
                       TimeSeries = {
                         object$TimeSeries = data.frame(timestamps = as.character(value$Dates), 
                                                        prices = as.numeric(value$Values),
                                                        stringsAsFactors = FALSE)
                       } )
              } else {
                warning(paste("field ", i, " does not exist, cannot assign value!", sep=""))
              }
            }
          })

## @include
#' @export
#' @rdname get-methods
#' @aliases get-method
#' @aliases get,RiskFactorConnector,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,YieldCurve,character-method
setMethod(f = "get", signature = c("ForeignExchangeRate", "character"),
          definition = function(object, what, ...){
            out <- list()
            if (length(what) == 1 && tolower(what) == "all") {
              what <- FEMS:::validFxRateGetFields()
            }
            for (i in what) {
              if (is.valid.fxrate.get.field(i)) {
                out[[i]] <- switch(i,
                                   MarketObjectCode = {
                                     object$MarketObjectCode
                                   },
                                   CurrencyPair = {
                                     object$CurrencyPair
                                   },
                                   Dates = {
                                     object$TimeSeries[,"timestamps"]
                                   },
                                   Values = {
                                     object$TimeSeries[,"prices"]
                                   },
                                   TimeSeries = list(Dates = get(object, "Dates"),
                                               Values = get(object, "Values"))
                )
              } else {
                warning(paste("field ", i, " does not exist, cannot get value!", sep=""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })

##############################################################
#' Generic method to retrieve the value of a 
#' \code{\link{ForeignExchangeRate,ReferenceIndex}} object at 
#' a certain point in time
#'
#' \code{\link{ForeignExchangeRate,ReferenceIndex}} risk
#' factors define the dynamics of the respective risk factor 
#' over time. \code{valueAt} computes or retrieves the value 
#' of this risk factor at a future point in time.
#' 
#' @param object An object of class \code{RiskFactor} for 
#'        which to return it's value at a given point in time
#'        
#' @param at The point in time at which to return the value
#' 
#' @param ...
#'
#' @return numeric The value of the respective risk factor
#'        at the specified point in time
#'
#' @examples
#' 
#' # create an FX-Rate object
#' fx <- FxRate() 
#' 
#' # define time stamps and values
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
#'            "2018-01-01", "2019-01-01")
#' values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
#' 
#' # set the MarketObjectCode and TimeSeries
#' set(fx, what = list(MarketObjectCode = "CHF/USD",
#'                     TimeSeries = list(Dates=times,Values=values)))  
#' 
#' # get the MarketObjectCode
#' get(fx, "MarketObjectCode")
#' 
#' # get values of the risk factor at certain times
#' valueAt(fx, "2016-01-01")
#' valueAt(fx, c("2016-01-01", "2018-07-01", "2018-07-01"))
#'
## @include
#' @export
#' @docType methods
#' @rdname vat-methods
## @aliases
setGeneric(name = "valueAt",
           def = function(object, at, ...){
             standardGeneric("valueAt")
           })

## @include
#' @export
#' @rdname vat-methods
#' @aliases vat-method
#' @aliases vat,ReferenceIndex,character-method
setMethod(f = "valueAt", signature = c("ForeignExchangeRate", "character"),
          definition = function(object, at, ...){
            datums <- sort(as.Date(unlist(object$TimeSeries["timestamps"])))
            bool_matrix <- t(sapply(at, function(x) datums <= as.Date(x)))
            indices <- unname(apply(bool_matrix,1,function(x) max(which(x))))
            return(object$TimeSeries["prices"][indices,1])
          })

## @include
#' @export
setMethod(f = "show", signature = c("ForeignExchangeRate"),
          definition = function(object){
            cat(paste0("MarketObjectCode: ", object$MarketObjectCode,"\n"))
            cat(paste0("CurrencyPair: ", object$CurrencyPair,"\n"))
            print("Time Series:")
            print(object$TimeSeries)
          })

## -----------------------------------------------------------------
## "private" helper methods
# existing fields in the java class
validFxRateSetFields <- function() {
  return(c(
    "TimeSeries", "MarketObjectCode", "CurrencyPair"
  ))
}
is.valid.fxrate.set.field <- function(x) {
  valid <- validFxRateSetFields()
  return(x %in% valid)
}
validFxRateGetFields <- function() {
  return(c(
    "MarketObjectCode", "CurrencyPair", "Dates", 
    "Values", "TimeSeries", "jref"
  ))
}
is.valid.fxrate.get.field <- function(x) {
  valid <- validFxRateGetFields()
  return(x %in% valid)
}
