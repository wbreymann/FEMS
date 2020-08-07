#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A \code{\ref ValuationEngine} implementing discounting of
#' future cash flows in different currencies
#' 
#' This class represents a \code{\link{ValuationEngine}} that
#' uses the future cash flow discounting method to compute a
#' value of two legs in different currencies and converts 
#' these in the desired (target) currency of a CT. Thereby, 
#' the cash flows to be valued may be in two different 
#' currencies each being modelled with a separate discounting
#' curve and discounting spread. Finally, based on a reference 
#' FX-rate index the values of the two currency-legs are 
#' converted into the specified target currency. The engine 
#' is implemented in Java so that this class points to an 
#' instance of the respective Java class. 
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{DiscountingEngine, ValuationEngine, 
#'                ValuationEngineModel}}
#'
## @examples
#' 
#' @include ValuationEngine.R
#' @export 
#' @rdname dcv-classes
setRefClass("MultiCurrencyDiscountingEngine",
            contains = c("ValuationEngine"),
            fields = list(
              CurrencyPair="character",
              targetCurrency="character",
              interestRatesObjectLinkCollection="list",
              spreadCollection="list"
            ))

## @include 
## @export
## @rdname
setRefClass("MultiCurrencyDiscountingModel",
            contains = c("ValuationEngineModel"),
            fields = list(
            ))

## @include
#' @export
#' @rdname vem-methods
setGeneric(name = "XfxDcModel",
           def = function(...){
             standardGeneric("XfxDcModel")
           })

## @include
#' @export
#' @rdname vem-methods
setMethod(f = "XfxDcModel",signature = c(),
          definition = function(...){
              model <- new("MultiCurrencyDiscountingModel")
              pars <- list(...)
              if(length(pars)==0){
              }else if(is.list(pars[[1]])){
                  set(object=model, what=pars[[1]])
              }else{
                  set(object=model, what=pars)
              }
              return(model)
          })

## @include
#' @export
#' @rdname ve-methods
setGeneric(name = "XfxDcEngine",
           def = function(...){
             standardGeneric("XfxDcEngine")
           })
## @include
#' @export
#' @rdname ve-methods
setMethod(f = "XfxDcEngine",signature = c(),
          definition = function(...){
              object = new("MultiCurrencyDiscountingEngine")
              pars = list(...)
              if(length(pars)==0){
              }else if(is.list(pars[[1]])){
                  set(object=object, what=pars[[1]])
              }else{
                  set(object=object, what=pars)
              }
              return(object)
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature="MultiCurrencyDiscountingModel",
          function(.Object, ...) {
              .Object <- callNextMethod()
              .Object$jref <- jobj
              return(.Object)
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature="MultiCurrencyDiscountingEngine",
          function(.Object, ...) {
              .Object <- callNextMethod()
              return(.Object)
          })

#' ## @include
#' #' @export
#' #' @rdname get-methods
#' #' @aliases get,ContractType,character-method
#' #' @aliases get,YieldCurve,character-method
#' #' @aliases get,ReferenceIndex,character-method
#' #' @aliases get,ForeignExchangeRate,character-method
#' #' @aliases get,ValuationEngine,character-method
#' #' @aliases get,jobjRef,character-method
#' setMethod(f = "get", signature = c("MultiCurrencyDiscountingEngine","character"),
#'           definition = function(object, what){
#'             model <- .jcall(object$jref, "Lorg/actus/misc/valuationmodels/MultiCurrencyDiscountingModel;",
#'                             "getModel")
#'             if(what=="Model") {
#'               out <- XfxDcModel()
#'               out$jref <- model
#'               return(out)
#'             } else if (what=="InterestRatesModel") {
#'                 jhelper <- try(.jnew("org/rfl/ractus/valuation/MultiCurrencyDiscountingEngineHelper"))
#'                 if(inherits(jhelper, "try-error")) {
#'                   stop("Java MultiCurrencyDiscountingEngineHelper instance could not be created!")
#'                 }
#'                 out <- .jcall(jhelper, "Lorg/rfl/ractus/riskfactors/YieldCurveHelper;",
#'                               "getInterestRateProvider", object$jref)
#'             } else {
#'               get(model, what)
#'             }
#'           })

#' #' @include RiskFactorConnector.R
#' #' @export
#' #' @rdname set-methods
#' #' @aliases set,ContractType,list-method
#' #' @aliases set,ContractType,ValuationEngine-method
#' #' @aliases set,ContractType,ContractModel-method
#' #' @aliases set,YieldCurve,list-method
#' #' @aliases set,ReferenceIndex,list-method
#' #' @aliases set,ForeignExchangeRate,list-method
#' #' @aliases set,ValuationEngineModel,list-method
#' setMethod(f = "set", signature = c("MultiCurrencyDiscountingEngine","list"),
#'           definition = function(object, what){
#'             set(rActus:::get(object, "Model"), what)
#'           })


#' @include RiskFactorConnector.R
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,ContractType,ValuationEngine-method
#' @aliases set,ContractType,ContractModel-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
setMethod(f = "set", signature = c("MultiCurrencyDiscountingModel", "list"),
          definition = function(object, what, ...){
            par.names <- names(what)
            for(i in par.names) {
              if(is.valid.xfxdcmodel.field(i)) {
                switch(i,
                       CurrencyPair = {
                         object$CurrencyPair=what[[i]]
                       },
                       TargetCurrency = {
                         object$targetCurrency=what[[i]]
                       },
                       InterestRateModelObjectLinks = {
                         interestRatesObjectLinkCollection_list=list(what[[i]][[1]],what[[i]][[2]])
                         names(interestRatesObjectLinkCollection_list)=c(names(what[[i]])[1],names(what[[i]])[2])
                         object$interestRatesObjectLinkCollection=interestRatesObjectLinkCollection_list
                       },
                       SpreadCollection = {
                         spreadCollection_list=list(what[[i]][[1]],what[[i]][[2]])
                         names(spreadCollection_list)=c(names(what[[i]])[1],names(what[[i]])[2])
                         object$spreadCollection=spreadCollection_list
                       }
                )
              } else {
                warning(paste("field ", i, " does not exist, cannot assign value!", sep=""))
              }
            }
          })
## -----------------------------------------------------------------
## private util methods
validXFxDcModelFields <- function() {
  return(terms(XfxDcModel()))
}
is.valid.xfxdcmodel.field <- function(x) {
  valid <- validXFxDcModelFields()
  return(x%in%valid)
}
validCurrency <- function(curr){
  currencies = list(
    "INR", "USD", "EUR", "JPY", "GBP", "CHF", "AUD", "CAD", "NZD", "DKK",
    "HKD", "SGD", "DEM", "BEF", "NOK", "ATS", "PLN", "SKK", "CNY", "SEK",
    "BRL", "CZK", "MXN", "RON", "NLG", "MYR" )
  return(curr%in% currencies)
}

versuch<- function(text){
 if(text=="1"){
   out=1
   return(out)
 } 
  else{
    out=2
  }
}

# a=list(4,2,1)
# names(a) <- c("dfj","df")