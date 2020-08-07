#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the liquidity-vector for \code{ContractType}
#'
#' Different liquidity-concepts can be derived for a financial instrument
#' or the resulting EventSeries, respectively. Currentently, these are
#' Marginal liquidity and Cumulative liquidity.
#'
#' Marginal liquidity-vector represents the aggregate cash flows within a
#' set of user-defined time-intervals. The time-intervals are defined as
#' a sequence of timeDate-dates. Thereby, the marginal liquidity-vector
#' gives the net cash flows within the specified time-intervals.
#'
#' Cumulative liquidity-vector is the cumulative sum over time (-intervals)
#' of the marginal liquidity-vector.
#'
#' @param object The \code{ContractType} or \code{EventSeries}-object for which to derive the liquidity-vector
#'
#' @param by A sequence of 'timeDate's providing the target time-axis for the liquidity-vector
#'
#' @param type A character representing the type of liquidity (either 'marginal' or 'cumulative')
#'  
#' @param ... Currently unused
#'  
#' @return A \code{numeric} object representing the liquidity-vector on the target time-axis
#' 
#' @seealso \code{\link{ContractType}} and \code{\link{EventSeries}}
#'
#' @examples
#' pam <- Pam()
#' set(pam, what=list(
#'                  ContractID = "001",
#'                  Currency = "CHF",
#'                  Calendar = "Weekday",
#'                  ContractRole = "RPA",               
#'                  StatusDate       = "2016-05-30T00",
#'                  ContractDealDate = "2016-05-30T00",
#'                  InitialExchangeDate = "2016-05-30T00",
#'                  MaturityDate = "2020-06-01T00",
#'                  NotionalPrincipal = 1000,
#'                  NominalInterestRate = 0.05,
#'                  CycleOfInterestPayment = "1Y-", 
#'                  PremiumDiscountAtIED = 0.0,
#'                  DayCountConvention = "30E/360",
#'                  BusinessDayConvention = "SCF"))
#' ad <- "2016-06-01T00"
#' 
#' # generate event series
#' evs = events(pam, ad)
#' 
#' # define target liquidity time axis
#' by=timeSequence(substring(ad, 1, 10), "2020-06-01", by="1 year")
#' 
#' # derive marginal liquidity for defined time axis
#' liquidity(evs, by, "marginal")
#' 
#' # derive cumulative liquidity for defined time axis
#' liquidity(evs, by, "cumulative")
#' 
## @include 
#' @export
#' @docType methods
#' @rdname liq-methods
setGeneric(name = "liquidity", def = function(object, by, type, ...){
  standardGeneric("liquidity")
})

#' @include ContractType.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("ContractType", "timeDate", "missing"),
          definition = function(object, by, type, digits = 2) {
            return(liquidity(object, by, type = "marginal", digits = digits))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("ContractType", "timeBuckets", "missing"),
          definition = function(object, by, type, digits = 2) {
            liq <- liquidity(object, as.timeDate(by), type = "marginal", digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })


#' @include ContractType.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("ContractType", "timeDate", "character"),
          definition = function(object, by, type, digits = 2) {
            if (type == "marginal") {
              liq <- liquidity(EventSeries(object, by[1]), by, type, digits=digits)
            } else if (type == "cumulative") {
              liq <- cumsum(liquidity(object, by, type = "marginal", digits = digits))
            } else {
              stop(paste("Liquidity type '", type, "' not recognized!", sep = ""))
            }
            return(liq)
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("ContractType", "timeBuckets", "character"),
          definition = function(object, by, type, digits=2) {
            liq <- liquidity(object, as.timeDate(by), type = type, digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Portfolio", "timeDate", "missing"),
          definition = function(object, by, ...){
            return(liquidity(object, by, type = "marginal", ...))
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Portfolio", "timeBuckets", "missing"),
          definition = function(object, by, ...){
            liq <- liquidity(object, as.timeDate(by), type = "marginal", ...)
            names(liq) <- tb@bucketLabs
            return(liq)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Portfolio", "timeBuckets", "character"),
          definition = function(object, by, type, ...){
            liq <- liquidity(object, as.timeDate(by), type, ...)
            names(liq) <- tb@bucketLabs
            return(liq)
          })


#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Portfolio", "timeDate", "character"),
          definition = function(object, by, type, digits = 2, ...){
            return(liquidity(events(object, as.character(by[1])), by, type, ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("EventSeries", "timeDate", "missing"),
          definition = function(object, by, type, digits = 2){
            return(liquidity(object, by, type = "marginal", digits = digits))
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("EventSeries", "timeBuckets", "missing"),
          definition = function(object, by, type, digits=2) {
            liq <- liquidity(object, as.timeDate(by), type = "marginal", digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })

#' @include EventSeries.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("EventSeries", "timeDate", "character"),
          definition = function(object, by, type, digits = 2){
            if (type == "marginal") {
              liq <- timeSeries(rep(0, length(by)), charvec = by)
              cf.raw <- timeSeries(get(object,"evs")$Payoff,
                                 charvec = substring(get(object,"evs")$Date, 1, 10))
              # cf.aggr = .aggregate.timeSeries(cf.raw, by, FUN=sum)
              cf.aggr <- aggregate(cf.raw, by, FUN = sum)
              
              liq[time(cf.aggr),] <- cf.aggr
              liq <- as.numeric(series(liq))[-1]
            } else if (type == "cumulative") {
              liq <- cumsum(liquidity(object, by, type = "marginal", digits = digits))
            } else {
              stop(paste("Liquidity type '", type, "' not recognized!", sep = ""))
            }
            return(round(liq, digits))
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("EventSeries", "timeBuckets", "character"),
          definition = function(object, by, type, digits = 2) {
            liq <- liquidity(object, as.timeDate(by), type = type, digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })
