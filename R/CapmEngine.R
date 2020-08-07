#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A \code{\link{ValuationEngine}} implementing the Capital
#' Asset Pricing Model
#' 
#' This class represents a \code{link{ValuationEngine}} that
#' uses Capital Asset Pricing Model (CAPM) method to compute
#' a value of a CT. The engine is implemented in Java so that
#' this class points to an instance of the respective Java
#' class. 
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
setRefClass("CAPMEngine",
            contains = c("ValuationEngine"),
            fields = list(
            ))

## @include 
## @export
## @rdname
setRefClass("CAPMModel",
            contains = c("ValuationEngineModel"),
            fields = list(
            ))

## @include
#' @export
#' @rdname vem-methods
setGeneric(name = "CapmModel",
           def = function(...){
             standardGeneric("CapmModel")
           })

## @include
#' @export
#' @rdname vem-methods
setMethod(f = "CapmModel",signature = c(),
          definition = function(...){
              model <- new("CAPMModel")
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
setGeneric(name = "CapmEngine",
           def = function(...){
             standardGeneric("CapmEngine")
           })

## @include
#' @export
#' @rdname ve-methods
setMethod(f = "CapmEngine",signature = c(),
          definition = function(...){
              object = new("CAPMEngine")
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
setMethod(f = "initialize", signature="CAPMModel",
          function(.Object, ...) {
              .Object <- callNextMethod()
              jobj <- try(.jnew("org/actus/misc/valuationmodels/CAPMModel"))
              if(inherits(jobj, "try-error")) {
                  stop("Java CAPMModel could not be instantiated!")
              }
              .Object$jref <- jobj
              return(.Object)
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature="CAPMEngine",
          function(.Object, ...) {
              .Object <- callNextMethod()
              jobjCT <- try(.jnew("org/actus/misc/valuationmodels/CAPMEngine"))
              jobjMod <- try(.jnew("org/actus/misc/valuationmodels/CAPMModel"))
              jhelper <- try(.jnew("org/rfl/ractus/valuation/CAPMEngineHelper"))
              if(inherits(jobjCT, "try-error")) {
                  stop("Java CAPMEngine instance could not be created!")
              }
              if(inherits(jobjMod, "try-error")) {
                  stop("Java CAPMModel instance could not be created!")
              }
              if(inherits(jhelper, "try-error")) {
                stop("Java CAPMEngineHelper instance could not be created!")
              }
              .jcall(jhelper, "V", "setModel", jobjCT, jobjMod)
              .Object$jref <- jobjCT
              return(.Object)
          })

## @include
#' @export
#' @rdname get-methods
#' @aliases get,ContractType,character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngine,character-method
#' @aliases get,jobjRef,character-method
setMethod(f = "get", signature = c("CAPMEngine","character"),
          definition = function(object, what){
            model <- .jcall(object$jref, "Lorg/actus/misc/valuationmodels/CAPMModel;",
                            "getModel")
            if(what=="Model") {
              out <- CapmModel()
              out$jref <- model
              return(out)
            } else {
              get(model, what)
            }
          })