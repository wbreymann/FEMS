#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class parent to all ACTUS Contract Types
#' 
#' This class is only used in an abstract sense in that it is
#' not intended to create instances of it. It's use is to 
#' serve as parent to various implementations of ACTUS CTs as 
#' a means for designing method-inheritage. 
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{PrincipalAtMaturity, Annuity, Option}}
#'
## @examples
#' 
#' @include ContractModel.R ValuationEngine.R RiskFactorConnector.R AnalysisDate.R ContractABC.R
#' @export
#' @rdname ct-classes
setRefClass("ContractType",
            contains = "ContractABC",
            fields = list(
              ContractTerms = "list",
              rf_connector = "RiskFactorConnector",
              ct_events = "data.frame",
              val_engine = "ValuationEngine"
            ))

##############################################################
#' \code{ContractType}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{ContractType} (e.g. \code{\link{PrincipalAtMaturity}},
#' \code{\link{Stock}}, etc). 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{Pam}} for 
#' \code{\link{PrincipalAtMaturity}}, \code{\link{Ann}} for 
#' \code{\link{Annuity}} or \code{\link{Stk}} for 
#' \code{\link{Stock}}. Note that it is not possible to
#' instanciate class \code{ContractType} itself but only the
#' implementing classes extending \code{ContractType}.
#' 
#' @param ... If a character, then \code{object}
#'        is expected to be the R-class name of the contract to
#'        be instantiated.
#'
#' @return An object of a class extending \code{ContractType} 
#' 
#' @seealso \code{\link{Pam, PrincipalAtMaturity}}
#'
#' @examples 
#' # example 1: create a new 'PAM' object
#' pam <- CT("PrincipalAtMaturity")
#' 
#' # example 2: attach the reference to a Java 'PAM' object to
#' #            a new R-'PAM' object. Note, the new object will
#' #            refer to the same Java contract.
#' pam <- Pam()
#' set(pam, list(ContractID = "001",
#'               Currency = "CHF",
#'               ContractRole = "RPA",
#'               StatusDate       = "2012-12-31",
#'               ContractDealDate = "2012-12-31",
#'               InitialExchangeDate = "2013-01-01",
#'               MaturityDate = "2013-03-31",
#'               NotionalPrincipal = 1000, 
#'               NominalInterestRate = 0.01,
#'               DayCountConvention = "30E360"))
#'
## @include
#' @export
#' @docType methods
#' @rdname ct-methods
#' @aliases CT,jobjRef-method
#' @aliases CT,character-method
setGeneric(name = "CT",
           def = function(contract_name){
             standardGeneric("CT")
           })

## @include
#' @export
#' @rdname ct-methods
#' @aliases CT,character-method
setMethod(f = "CT", signature = c("character"),
          definition = function(contract_name) {
            if (!contract_name %in% names(actusDictionary$rflActus_attributes)) {
              stop(paste("ErrorIn::ContractType:: Type of Contract ", 
                         contract_name, " does not exist !!!"))
            }
            out <- new(contract_name)
            return(out)
          })

## @include
#' @export
#' @rdname trms-methods
#' @aliases terms,ContractModel-method
#' @aliases terms,ContractType-method
#' @aliases terms,ValuationEngine-method
#' @aliases terms,jobjRef-method
setMethod(f = "terms", signature = c("ContractType"),
          definition = function(object) {
            return(names(object$ContractTerms))
          })


## @include
#' @export
#' @rdname get-methods
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngineModel,character-method
#' @aliases get,jobjRef,character-method
setMethod(f = "get", signature = c("ContractType", "character"),
          definition = function(object, what){
            
            if (what[1] == "EventSeries" || what[1] == "ProcessedEventSeries") {
              # out <- .jcall(object$jref,
              #               "Lorg/actus/util/time/EventSeries;",
              #               "getProcessedEventSeries")
            } else if (what[1] == "GeneratedEventSeries") {
              # out <- .jcall(object$jref,
              #               "Lorg/actus/util/time/EventSeries;",
              #               "getGeneratedEventSeries")
            } else if (what[1] == "RiskFactorConnector") {
              out <- object$rf_connector
            } else {
              # out <- object$ContractTerms[[what]]
              out <- as.list(sapply(what, function(x) object$ContractTerms[[x]]))
            }
            return(out)
          })


#' @include ValuationEngine.R
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,ContractType,ContractModel-method
#' @aliases set,ContractType,RiskFactorConnector-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
setMethod(f = "set", signature = c("ContractType", "ValuationEngine"),
          definition = function(object, what){
            object$val_engine <- what
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,ValuationEngine-method
#' @aliases set,ContractType,ContractModel-method
#' @aliases set,ContractType,RiskFactorConnector-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
setMethod(f = "set", signature = c("ContractType", "list"),
          definition = function(object, what){
            for (i in 1:length(what)) {
              object$ContractTerms[names(what[i])] <- what[[i]]
            }
            details <- getContractModel(object)
            checkArguments(details, what)
          })

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
setMethod(f = "set", signature = c("ContractType", "RiskFactorConnector"),
          definition = function(object, what){
            object$rf_connector <- what
          })

## @include
# #' @export
# setMethod("show", signature = "ContractType",
#           definition = function(object){
#             df <- t(as.data.frame(object$ContractTerms))
#             colnames(df) <- "ContractTerms"
#             print(df)
#           })


## @include
# #' @export
# setMethod("summary", signature=c(object = "ContractType"),
#   definition = function(object){
#     x = as.data.frame(object$ContractTerms)
#     cat(paste("ContractType: ", as.character(x["ContractType"]), "\n", sep=""))
#     cat(paste("ContractID: ", as.character(x["ContractID"]), "\n", sep=""))
#   })

##############################################################
#' \code{ContractType}-subscript methods
#'
#' accesses elements of an \code{ContractType}
#'
#' This method accesses elements of an object of class \code{ContractType} 
#'
## @include
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("ContractType", "character", "missing"),
          definition = function(x, i) {
            z = as.data.frame(x$ContractTerms)
            # id <- is.element(x$evs[,1], i)
            z[i]
          }
)

## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("ContractType", "numeric", "missing"),
          definition = function(x, i) {
            z = as.data.frame(x$ContractTerms)
            z[i]  
          }
)

## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("ContractType", "logical", "missing"),
          definition = function(x,i) {
            z = as.data.frame(x$ContractTerms)
            z[i]  
          }
)


## @include
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[<-", signature = c("ContractType", "character", "missing", "ANY"),
          definition = function(x, i, value) {
            x$ContractTerms[[i]] = value
            return(x)
          }
)

## @include
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[<-", signature = c("ContractType", "numeric", "missing", "ANY"),
          definition = function(x, i, value) {
            x$ContractTerms[[i]] = value
            return(x)
          }
)


setGeneric(name = "getContractModel",
           def = function(object,...){
             standardGeneric("getContractModel")
           })
#' @export
setMethod(f = "getContractModel",signature = c("ContractType"),
          definition = function(object,...){
            long_name <- longName(tolower(object$ContractTerms$ContractType))
            return(CTM(long_name))
          })

