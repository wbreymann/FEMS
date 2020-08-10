#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include CompoundContract.R
#' @export
#' @rdname ct-classes
setRefClass("Swap",
            contains = c("CompoundContract"),
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Swaps",
           def = function(...){
             standardGeneric("Swaps")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Swaps",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType <- "SWAPS"
    object <- new("Swap")
    set(object = object, what = pars)
    return(object)
  }
)

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "Swap",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("Swap")$ContractTerms, function(x) {"NULL"})
              .Object$ContractTerms <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "Swap",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Swap")))
#           })

#' #' @include ContractType.R
#' #' @export
#' #' @rdname set-methods
#' #' @aliases set,ContractType,list-method
#' #' @aliases set,ContractType,ValuationEngine-method
#' #' @aliases set,ContractType,RiskFactorConnector-method
#' #' @aliases set,YieldCurve,list-method
#' #' @aliases set,ReferenceIndex,list-method
#' #' @aliases set,ForeignExchangeRate,list-method
#' #' @aliases set,ValuationEngineModel,list-method
#' setMethod(f = "set", signature = c("Swap","ContractType"),
#'           definition = function(object, what, ...){
#'               jHelper <- .jnew("org/rfl/ractus/contracttypes/ChildContractHelper")
#'               .jcall(jHelper, "V", "newChildSet", object$jref)
#'               .jcall(jHelper, "V", "addChildContract", object$jref, what$jref)
#'               .jcall(jHelper, "V", "addChildContract", object$jref,
#'                      list(...)[[1]]$jref)
#'           })
#' 
#' 
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
#' setMethod(f = "set", signature = c("Swap", "RiskFactorConnector"),
#'           definition = function(object, what){
#'             .jcall(object$jref, "V", "setRiskFactors", what$jref)
#'             # set risk factors to child contracts
#'             jHelper <- .jnew("org/rfl/ractus/contracttypes/ChildContractHelper")
#'             .jcall(jHelper,"V", "setRiskFactors", object$jref,what$jref)
#'           })

#swap_example=Swaps()
#getModelDetails(swap_example)