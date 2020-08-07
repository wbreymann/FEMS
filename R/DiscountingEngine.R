#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A \code{\ref ValuationEngine} implementing discounting of
#' future cash flows
#' 
#' This class represents a \code{\link{ValuationEngine}} that
#' uses the future cash flow discounting method to compute a
#' value of a CT. The engine is implemented in Java so that
#' this class points to an instance of the respective Java
#' class. 
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{DiscountingEngineModel, ValuationEngine, 
#'                ValuationEngineModel}}
#'
## @examples
#' 
#' @include ValuationEngine.R
#' @include YieldCurve.R
#' @export 
#' @rdname dcv-classes
setRefClass("DiscountingEngine",
            contains = c("ValuationEngine"),
            fields = list(DiscountingSpread = "numeric",
                          RiskFactorObject = "YieldCurve"
            ))

## @include 
## @export
## @rdname
# setRefClass("DiscountingModel",
#             contains = c("ValuationEngineModel"),
#             fields = list(
#             ))

## @include
#' @export
#' @rdname vem-methods
setGeneric(name = "DcModel",
           def = function(...){
             standardGeneric("DcModel")
           })

## @include
#' @export
#' @rdname vem-methods
# setMethod(f = "DcModel",signature = c(),
#           definition = function(...){
#               model <- new("DiscountingModel")
#               pars <- list(...)
#               if(length(pars)==0){
#               }else if(is.list(pars[[1]])){
#                   set(object=model, what=pars[[1]])
#               }else{
#                   set(object=model, what=pars)
#               }
#               return(model)
#           })

## @include
#' @export
#' @rdname ve-methods
setGeneric(name = "DcEngine",
           def = function(rf, ...){
             standardGeneric("DcEngine")
           })

## @include
#' @export
#' @rdname ve-methods
setMethod(f = "DcEngine", signature = c("missing"),
          definition = function(...){
              object <- new("DiscountingEngine")
              pars <- list(...)
              if (length(pars) == 0) {
              } else if (is.list(pars[[1]])) {
                  set(object = object, what = pars[[1]])
              } else {
                  set(object = object, what = pars)
              }
              return(object)
          })

## @include
#' @export
#' @rdname ve-methods
setMethod(f = "DcEngine", signature = c("RiskFactorConnector"),
          definition = function(rf, ...){
            object <- DcEngine(...)
            set(object, rf)
            return(object)
          })

setMethod(f = "set",
          signature = c("DiscountingEngine","list"),
          definition = function(object, what) {
            par.names <- names(what)
            for (i in par.names) {
              if (is.valid.dcengine.field(i)) {
                value <- what[[i]]
                switch(i,
                       RiskFactorObject = {
                         object$RiskFactorObject <- value
                       },
                       DiscountingSpread = {
                         object$DiscountingSpread <- value
                       }
                )
              } else {
                warning(paste("ErrorInDiscountingEngine:: Field ", i, " does not exist, cannot assign value!", sep = ""))
              }
            }
          })

setMethod(f = "set",
          signature = c("DiscountingEngine","RiskFactorConnector"),
          definition = function(object, what) {
            # get the yield curve out and give an error if more than one yield curve is defined
            is_yc <- unlist(lapply(rf$riskfactors, function(x) class(x) == "YieldCurve"))
            if (sum(as.numeric(is_yc)) > 1) {
              stop("ErrorIn::DiscountingEngine:: Only one YieldCurve can be defined in RiskFactorConnector for DCEngine !!!")
            }
            object$RiskFactorObject <- rf$riskfactors[[is_yc]]
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
# setMethod(f = "initialize", signature="DiscountingModel",
#           function(.Object, ...) {
#               .Object <- callNextMethod()
#               jobj <- try(.jnew("org/actus/misc/valuationmodels/DiscountingModel"))
#               if(inherits(jobj, "try-error")) {
#                   stop("Java DiscountingModel could not be instantiated!")
#               }
#               .Object$jref <- jobj
#               return(.Object)
#           })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
# setMethod(f = "initialize", signature = "DiscountingEngine",
#           function(.Object, ...) {
#               .Object <- callNextMethod()
#               # jobjCT <- try(.jnew("org/actus/misc/valuationmodels/DiscountingEngine"))
#               # jobjMod <- try(.jnew("org/actus/misc/valuationmodels/DiscountingModel"))
#               # jhelper <- try(.jnew("org/rfl/ractus/valuation/DiscountingEngineHelper"))
#               # if(inherits(jobjCT, "try-error")) {
#               #     stop("Java DiscountingEngine instance could not be created!")
#               # }
#               # if(inherits(jobjMod, "try-error")) {
#               #     stop("Java DiscountingModel instance could not be created!")
#               # }
#               # if(inherits(jhelper, "try-error")) {
#               #   stop("Java DiscountingEngineHelper instance could not be created!")
#               # }
#               # .jcall(jhelper, "V", "setModel", jobjCT, jobjMod)
#               # .Object$jref <- jobjCT
#               return(.Object)
#           })

## @include
#' @export
#' @rdname get-methods
#' @aliases get,ContractType,character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngine,character-method
#' @aliases get,jobjRef,character-method
setMethod(f = "get", signature = c("DiscountingEngine","character"),
          definition = function(object, what){
            if (what == "RiskFactorObject") {
                out <- object$RiskFactorObject
              } else if (what == "DiscountingSpread") {
                out <- object$DiscountingSpread
              }
            return(out)
          })

## @include
#' @export
setMethod("show", signature = "DiscountingEngine",
          definition = function(object){
            cat(paste0("DiscountingSpread: ", object$DiscountingSpread,"\n"))
            print("RiskFactorObject:")
            print(object$RiskFactorObject)
          })

## -----------------------------------------------------------------
## helper methods
# existing fields in the DiscountingEngine class
validDCEngineFields <- function() {
  return(c("RiskFactorObject", "DiscountingSpread"))
}

# check if fields are valid
is.valid.dcengine.field <- function(x) {
  valid <- validDCEngineFields()
  return(x %in% valid)
}


