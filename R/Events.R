#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the events for a \code{ContractType}
#'
#' The events of a contract are directly derived from the legal obligations 
#' defined therein. Events mark the exchange of a cash flow among the parties
#' involved in the contract or changes in the inner states of the contract
#' affecting cash flows to be exchanged in future. As such, events build the
#' basic analytical results upon which many financial analyses build.
#'
#' @param object The \code{ContractType} for which to derive the events
#'
#' @param ad The analysis date as per which all future events are to be derived
#'
#' @param model (optional) The \code{RiskFactorConnector} conditional to which events are computed
#'  
#' @return A \code{EventSeries} object containing the resulting events
#' 
#' @seealso \link{ContractType}, \link{EventSeries}, \link{RiskFactorConnector}
#'
#' @examples
#' # define the contract
#' pam = Pam(
#'            what=list(
#'                  ContractID = "001",
#'                  Currency = "CHF",
#'                  ContractRole = "RPA",               
#'                  StatusDate       = "2016-05-30T00",
#'                  ContractDealDate = "2016-05-30T00",
#'                  InitialExchangeDate = "2016-05-30T00",
#'                  MaturityDate = "2020-06-01T00",
#'                  NotionalPrincipal = 1000,
#'                  NominalInterestRate = 0.05,
#'                  CycleOfInterestPayment = "1Y-",
#'                  CycleOfRateReset = "1Y-",
#'                  MarketObjectCodeRateReset = "RiskFreeYC",
#'                  DayCountConvention = "30E/360"))
#'                  
#' # define the analysis date
#' ad = "2016-06-01T00"
#' 
#' # define the risk factor connector
#' yc = YieldCurve(
#' what = list(MarketObjectCode = "RiskFreeYC",
#'   Nodes = list(ReferenceDate = ad, 
#'   Tenors = c("1W", "1M", "6M", "1Y", "2Y", "5Y"), 
#'   Rates = c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03))))
#' rf <- RFConn()
#' add(rf,yc)
#' 
#' # compute events
#' evs=events(pam,ad,rf)
#' evs
#' as.data.frame(evs)
#' 
## @include 
#' @export
#' @docType methods
#' @rdname ev-methods
setGeneric(name = "events", def = function(object, ad, model, ...){
  standardGeneric("events")
})

#' @include ContractType.R
#' @include AnalysisDate.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("ContractType", "character", "missing"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad)))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include EventSeries.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("ContractType", "AD0", "missing"),
          definition = function(object, ad, model){
            return(EventSeries(object, ad))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("ContractType", "character", "RiskFactorConnector"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad), model))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include EventSeries.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("ContractType", "AD0", "RiskFactorConnector"),
          definition = function(object, ad, model){
            set(object, model)
            return(EventSeries(object, ad))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include Portfolio.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "character", "missing"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad)))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include EventSeries.R
#' @include Portfolio.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "AD0", "missing"),
          definition = function(object, ad, model){
            return(EventSeries(object, ad))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include Portfolio.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "character", "RiskFactorConnector"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad), model))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include EventSeries.R
#' @include Portfolio.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "AD0", "RiskFactorConnector"),
          definition = function(object, ad, model){
            set(object, model)
            return(EventSeries(object, ad))
          })