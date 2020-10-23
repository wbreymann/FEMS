#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that represents all Valuation Engine objects
#' 
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to 
#' serve as parent to various implementations of Valuation
#' Engine types as a means for designing method-inheritage. 
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{ValuationEngineModel, DiscountingEngine, 
#'                CAPMEngine, MultiCurrencyDiscountingEngine}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname ve-classes
setRefClass("ValuationEngine",
            fields = list(
            ))

##############################################################
#' A Reference Class that represents all ValuationEngine
#' Models objects
#' 
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to 
#' provide the class structure, i.e. field definitions, to 
#' various implementations of Contract Model types and as a 
#' means for designing method-inheritage. 
#' 
#' Generally, a Valuation Engine Model has one field that 
#' contains a reference to a java object of the same class in 
#' the java ACTUS library (e.g. for 
#' \code{\link{DiscountingModel}} the referenced 
#' object will be of class 
#' org.actus.misc.valuation.DiscountingModel). Note that
#' the object "carrying" the attribute values, i.e. the
#' ValuationEngineModel object, lives in Java, or the JVM 
#' respectively, and not in R. Parameter values may be 
#' accessed/changed using methods 
#' \code{\link{get}}/\code{\link{set}} which uses the JNI
#' interface through R-package rJava.
#' 
#' For a list of available engine attributes use 
#' method \code{\link{terms}}.
#'  
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{ValuationEngine, 
#' DiscountingEngineModel}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname vem-classes
setRefClass("ValuationEngineModel",
            contains = c(),
            fields = list(
##            jref = "jobjRef" # This field must be replaced
            ))

##############################################################
#' \code{ValuationEngine}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{ValuationEngine} (e.g. \code{\link{DiscountingEngine}},
#' etc). 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{DcEngine}} for 
#' \code{\link{DiscountingEngine}}. Note that it is not possible 
#' to instanciate class \code{ValuationEngine} itself but only 
#' the implementing classes extending \code{ValuationEngine}.
#' 
#' @param object If an object of class \code{jobjRef}, then 
#'        this parameter is expected to be a reference to a Java
#'        valuation engine and a new R-object of the same class is 
#'        created where the Java reference is attached as the
#'        classes' \code{jref}-field. If a character, then
#'        \code{object} is expected to be the R-class name of
#'        the valuation engine to be instantiated.
#'
#' @return An object of a class extending \code{ValuationEngine} 
#' 
#' @seealso \code{\link{VEM, DcEng}}
#'
#' @examples 
#' # example 1: create a new 'DiscountingEngine' object
#' dc = VE("DiscountingEngine")
#' 
#' # example 2: attach the reference to a Java 'DiscountingEngine'
#' #            object to a new R-'DiscountingEngine' object. Note,
#' #            the new object will refer to the same Java engine.
#' dc = VE("DiscountingEngine")
#' set(dc, what=list(
#'                    RiskFactorObjectLink="YC_CHF",
#'                    dc.spread=0.05))
#' # same.dc = VE(dc$jref)   # This command doesn't work
#'
## @include
#' @export
#' @docType methods
#' @rdname ve-methods
#' @aliases VE,jobjRef-method
#' @aliases VE,character-method
setGeneric(name = "VE",
           def = function(object){
             standardGeneric("VE")
           })

## @include
#' @export
#' @docType methods
#' @rdname ve-methods
#' @aliases VE,jobjRef-method
setMethod(f = "VE", signature = c("character"),
          definition = function(object) {
            out <- new(object)
            return(out)
          })

##############################################################
#' \code{ValuationEngineModel}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{ValuationEngineModel} (e.g. 
#' \code{\link{DiscountingEngineModel}}, etc). 
#' 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{DcModel}} for 
#' \code{\link{DiscountingEngineModel}}. Note 
#' that it is not possible to instanciate class 
#' \code{DiscountingEngineModel} itself but only the implementing 
#' classes extending class \code{DiscountingEngineModel}.
#' 
#' @param object If an object of class \code{jobjRef}, then 
#'        this parameter is expected to be a reference to a Java
#'        discounting engine model and a new R-object of the same 
#'        class is created where the Java reference is attached 
#'        as the classes' \code{jref}-field. If a character, then 
#'        \code{object} is expected to be the R-class name of the 
#'        valuation engine for which a valuation engine model is
#'        to be instantiated.
#'
#' @return An object of a class extending \code{DiscountingEngineModel} 
#' 
#' @seealso \code{\link{ValuationEngineModel,DiscountingEngine}}
#'
#' @examples 
#' # example 1: create a new 'DiscountingEngine' object
#' dcm = VEM("DiscountingEngine")
#' 
#' # example 2: attach the reference to a Java 'DiscountingEngineModel'
#' #            object to a new R-'DiscountingEngineModel' object. Note,
#' #            the new object will refer to the same Java model.
#' dcm = VEM("DiscountingEngine")
#' set(dcm, what=list(
#'                    RiskFactorObjectLink="YC_CHF",
#'                    dc.spread=0.05))
#' # same.dcm = VEM(dcm$jref)   # This command doesn't work.
#' 
#' # example 3: create a new 'DiscountingEngineModel' object using the
#' #            Engine-specific model constructor method
#' dcm = DcModel()
#'
## @include
#' @export
#' @docType methods
#' @rdname vem-methods
#' @aliases VEM,jobjRef-method
#' @aliases VEM,character-method
setGeneric(name = "VEM",
           def = function(object){
             standardGeneric("VEM")
           })

## @include
#' @export
#' @rdname trms-methods
#' @aliases terms,ContractType-method
#' @aliases terms,ValuationEngineModel-method
#' @aliases terms,jobjRef-method
setMethod(f = "terms", signature = c("ValuationEngine"),
          definition = function(object) {
            return(names(object$getRefClass()$fields()))
          })

## @include
#' @export
#' @rdname trms-methods
#' @aliases terms,ContractType-method
#' @aliases terms,ValuationEngine-method
#' @aliases terms,jobjRef-method
# setMethod(f = "terms", signature = c("ValuationEngineModel"),
#           definition = function(object) {
#               return(terms(object$jref))
#           })

## @include
#' @export
#' @rdname get-methods
#' @aliases get, ContractType, character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngine,character-method
#' @aliases get,jobjRef,character-method
# setMethod(f = "get", signature = c("ValuationEngineModel","character"),
#           definition = function(object, what){
#               get(object$jref,what)
#           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
# setMethod(f = "set",
#           signature = c("ValuationEngineModel","jobjRef"),
#           definition = function(object, what){
#               object$jref <- what
#           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,jobjRef-method
# setMethod(f = "set",
#           signature = c("ValuationEngineModel","list"),
#           definition = function(object, what){
#               set(object$jref, what)
#           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
#' @aliases set,ValuationEngineModel,jobjRef-method
# setMethod(f = "set", signature = c("ValuationEngine","ValuationEngineModel"),
#           definition = function(object, what){
#               .jcall(object$jref, "V", "setModel", what$jref)
#           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
#' @aliases set,ValuationEngineModel,jobjRef-method
#' @aliases set,ValuationEngineModel,ValuationEngineModel-method
# setMethod(f = "set", signature = c("ValuationEngine","list"),
#           definition = function(object, what){
#               set(FEMS:::get(object, "Model")$jref, what)
#           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
#' @aliases set,ValuationEngineModel,jobjRef-method
#' @aliases set,ValuationEngineModel,ValuationEngineModel-method
#' @aliases set,ValuationEngine,list-method
# setMethod(f = "set", signature = c("ValuationEngine", "RiskFactorConnector"),
#           definition = function(object, what){
#             .jcall(object$jref, "V", "setRiskFactors", what$jref, 
#                    FEMS:::get(object, "Model")$jref)
#           })

## @include
#' @export
# setMethod("show", signature = "ValuationEngineModel",
#           definition = function(object){
#               print(get(object, "all"))
#           })

## @include
#' @export
# setMethod("show", signature = "ValuationEngine",
#           definition = function(object){
#               print(get(object, "all"))
#           })
