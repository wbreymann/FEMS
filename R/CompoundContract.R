#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that extends \code{"ContractType"} 
#' representing all Compound Contracts
#' 
#' Class \link{CompoundContract} is superclass to all compound
#' (aka combined) Contract Types in the ACTUS taxonomy (cf. 
#' \link{www.projectactus.org}).
#' 
#' @seealso \code{\link{ContractType, Swap}}
#'
## @examples
#' 
#' @include ContractType.R
#' @export 
#' @rdname ct-classes
setRefClass("CompoundContract",
            contains = "ContractType",
            fields = list(
            ))

## @include
#' @export
#' @rdname get-methods
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngineModel,character-method
#' @aliases get,jobjRef,character-method
# setMethod(f = "get", signature = c("CompoundContract","character"),
#           definition = function(object, what){
#             if(what[1]=="ChildContracts") {
#               out <- list()
#               jobj <- .jcall(object$jref,
#                              "Lorg/actus/contracttypes/ContractCollection;",
#                              "getChildContracts")
#               keys <- .jcall(jobj, "[S", "getIDs")
#               if(length(keys) == 0) {
#                 out[["NA"]] = "no child contract attached"
#               } else {
#                 for(i in 1:length(keys)) {
#                   o.raw <- .jcall(jobj, "Lorg/actus/contracttypes/ContractType;",
#                                   "getContractType", keys[i])
#                   o.cast <- .jcast(o.raw, .jclass(o.raw))
#                   out[[keys[i]]] <- CT(o.cast)
#                 }
#               }
#             } else if(what[1]=="ChildContractTypes") {
#               out <- character()
#               contracts <- get(object, "ChildContracts")
#               if(class(contracts[[1]])!="character") {
#                 for(i in 1:length(contracts)) {
#                   out[i] <- get(contracts[[i]], "ContractType")
#                 }
#               }
#             } else if(what[1]=="EventSeries" || what[1]=="ProcessedEventSeries") {
#               out <- .jcall(object$jref,
#                             "Lorg/actus/util/time/EventSeries;",
#                             "getProcessedEventSeries")
#             } else if(what[1]=="GeneratedEventSeries") {
#               out <- .jcall(object$jref,
#                             "Lorg/actus/util/time/EventSeries;",
#                             "getGeneratedEventSeries")
#             } else if(what[1]=="ValuationEngine") {
#               out <- .jcall(object$jref, "Lorg/actus/valuation/ValuationProvider;",
#                             "getValuationEngine")
#             } else {
#               #             jhelper <- .jnew("org/rfl/ractus/contracttypes/ContractModelHelper")
#               #             jobj <- .jcall(jhelper, paste("Lorg/actus/models/",
#               #                                                 class(object),
#               #                                                 "Model;",sep=""),
#               #                            "getContractModel", object$jref)
#               # -> since ACTUS 0.9.0 not necessary to use the helper class anymore
#               jobj <- .jcall(object$jref, "Lorg/actus/models/ContractModel;",
#                              "getContractModel")
#               out <- get(object=jobj, what=what)
#             }
#             return(out)
#           })

#' @include ContractType.R
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,ContractType,ValuationEngine-method
#' @aliases set,ContractType,RiskFactorConnector-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
# setMethod(f = "set", signature = c("CompoundContract","ContractType"),
#           definition = function(object, what){
#             jHelper <- .jnew("org/rfl/ractus/contracttypes/ChildContractHelper")
#             .jcall(jHelper, "V", "setChildContract", object$jref, what$jref)
#           })


## @include
#' @export
# setMethod("show", signature = "CompoundContract",
#           definition = function(object){
#             model <- new("ContractModel")
#             #             jhelper <- .jnew("org/rfl/ractus/contracttypes/ContractModelHelper")
#             #             jobj <- .jcall(jhelper, paste("Lorg/actus/models/",
#             #                                                 class(object),
#             #                                                 "Model;",sep=""),
#             #                            "getContractModel", object$jref)
#             # -> since ACTUS 0.9.0 not necessary to use the helper class anymore
#             model$jref <- .jcall(object$jref, "Lorg/actus/models/ContractModel;",
#                                  "getContractModel")
#             show(model)
#             get(object, "ChildContracts")
#           })