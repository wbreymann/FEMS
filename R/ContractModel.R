#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that represents all Contract Models objects
#' 
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to 
#' provide the class structure, i.e. field definitions, to 
#' various implementations of Contract Models for specific 
#' Contract Types and as a means for designing method-inheritage. 
#' 
#' Generally, a Contract Model has one field that contains a 
#' reference to a java object of the same class in the java 
#' ACTUS library (e.g. for 
#' \code{\link{PrincipalAtMaturityModel}} the referenced 
#' object will be of class 
#' org.actus.models.PrincipalAtMaturityModel). Note that
#' the object "carrying" the attribute values, i.e. the
#' Contract Model object, lives in Java, or the JVM 
#' respectively, and not in R. Attribute values may be 
#' accessed/changed using methods 
#' \code{\link{get}}/\code{\link{set}} which uses the JNI
#' interface through R-package rJava.
#' 
#' For a list of available contract ContractTerms use 
#' method \code{\link{terms}}. For an explanation of the 
#' ContractTerms refer to the official ACTUS data dictionary
#' under \link{www.projectactus.org}.
#' 
#' @field jref A rJava java object reference
#' 
#' @seealso \code{\link{ContractType, PrincipalAtMaturityModel, 
#'                AnnuityModel, StockModel}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname ctm-classes
setRefClass("ContractModel",
            fields = list(
              contract_type = "character",
              ContractTerms = "list",
              required = "list",
              allowed = "list"
            )
)

##############################################################
#' \code{ContractModel}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{ContractModel} (e.g. 
#' \code{\link{PrincipalAtMaturityModel}}, 
#' \code{\link{StockModel}}, etc). 
#' 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{PamModel}} for 
#' \code{\link{PrincipalAtMaturityModel}} or 
#' \code{\link{StkModel}} for \code{\link{StockModel}}. Note 
#' that it is not possible to instanciate class 
#' \code{ContractModel} itself but only the implementing 
#' classes extending \code{ContractModel}.
#' 
#' @param object If an object of class \code{jobjRef}, then 
#'        this parameter is expected to be a reference to a Java
#'        contract model and a new R-object of the same class is 
#'        created where the Java reference attached as the 
#'        classes' \code{jref}-field. If a character, then 
#'        \code{object} is expected to be the R-class name of the 
#'        CT for which a contract model is to be instantiated.
#'
#' @return An object of a class extending \code{ContractModel} 
#' 
#' @seealso \code{\link{ContractModel,PrincipalAtMaturityModel}}
#'
#' @examples 
#' # example 1: create a new 'PAM-Model' object
#' # This example does not work
#' # pam.model = CTM("PrincipalAtMaturity")
#' 
#' # example 2: attach the reference to a Java 'PAM-Model' object 
#' #            to a new R-'PAM' object. Note, the new object will
#' #            refer to the same Java contract.
#' # This example does not work
#' # pam.model = CTM("PrincipalAtMaturity")
#' # set(pam.model, what=list(
#' #                    ContractID = "001",
#' #                    Currency = "CHF",
#' #                    ContractRole = "RPA",
#' #                    StatusDate       = "2012-12-31T00",
#' #                    ContractDealDate = "2012-12-31T00",
#' #                    InitialExchangeDate = "2013-01-01T00",
#' #                    MaturityDate = "2013-03-31T00",
#' #                    NotionalPrincipal = 1000, 
#' #                    NominalInterestRate = 0.01,
#' #                    DayCountConvention = "30E/360"))
#' # same.pam.model = CTM(pam.model$jref)
#' 
#' # example 3: create a new 'PAM-Model' object using the
#' #            CT-specific model constructor method
#' pam.model = PamModel()
#'
## @include
#' @export
#' @docType methods
#' @rdname ctm-methods
#' @aliases CTM,jobjRef-method
#' @aliases CTM,character-method
setGeneric(name = "CTM",
           def = function(model){
             standardGeneric("CTM")
           })

## @include
#' @export
#' @rdname ctm-methods
#' @aliases CTM, character-method
setMethod(f = "CTM", signature = c("character"),
          definition = function(model) {
            
            if (!model %in% names(ActusDictionary$rflActus_attributes)) {
              stop(paste("ErrorIn::ContractModel:: Type of Contract ", model, " does not exist !!!"))
            }
            ctm <- new("ContractModel")
            
            ctm$contract_type <- model
            ctm$ContractTerms <- ActusDictionary$rflActus_attributes[[model]]
            ctm$required <- ActusDictionary$rflActus_required[[model]]
            ctm$allowed <- ActusDictionary$rflActus_allowed_vals[[model]]
            return(ctm)
          })

##############################################################
#' Generic method to return existing terms or parameters of an
#' ACTUS object
#'
#' Implemented e.g. for ACTUS contract models where \code{terms}
#' returns all available contract terms or for valuation 
#' engine models where the method returns the valuation model
#' parameters.
#' 
#' @param object The object for which to return terms
#'
#' @return character A character vector of the names of 
#'        available terms
#' 
#' @seealso \code{\link{get,set}}
#'
#' @examples
#' pam = Pam()
#' terms(pam) # returns all Principal At Maturity terms
#' 
#' dc = DcModel()
#' terms(dc) # returns all parameters of the discounting model
#' 
## @include
#' @export
#' @docType methods
#' @rdname trms-methods
#' @aliases terms,ContractType-method
#' @aliases terms,ValuationEngine-method
setGeneric(name = "terms", useAsDefault = TRUE,
           def = function(object){
             standardGeneric("terms")
           })

## @include
#' @export
#' @rdname trms-methods
#' @aliases terms,ContractType-method
#' @aliases terms,ValuationEngine-method
#' @aliases terms,jobjRef-method
setMethod(f = "terms", signature = c("ContractModel"),
          definition = function(object) {
            return(names(object$ContractTerms))
          })

##############################################################
#' Generic method to return the value of certain term(s) or
#' parameter(s) of an ACTUS object
#'
#' Implemented e.g. for \code{\link{ContractModel}} where 
#' \code{get} returns the value of defined contract term(s) or 
#' for \code{\link{RiskFactor}}, \code{\link{ValuationModel}}, 
#' and whatever object of this package that is carrying 
#' parameters.
#' Note that argument \code{what} has to refer to the exact
#' names of the terms or parameters to retrieve. Whenever 
#' possible, the method converts the values to an R data
#' format before returning. However, some terms (e.g.
#' DayCountConvention for a PrincipalAtMaturity) cannot be
#' converted why the Java reference is returned. 
#' 
#' @param object The object for which to return the term value(s)
#' 
#' @param what A character (vector) providing the term names for
#'        whose value(s) to return
#'        
#' @param ...
#'
#' @return A single element or vector of elements representing
#'        the values of the requested terms
#' 
#' @seealso \code{\link{terms,set}}
#'
#' @examples
#' pam = Pam()
#' terms(pam) # get all term names
#' get(pam, what="NotionalPrincipal") # return value is numeric
#' get(pam, what="DayCountConvention") # Java reference is returned
#' 
## @include
#' @export
#' @docType methods
#' @rdname get-methods
#' @aliases get,ContractType,character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngineModel,character-method
#' @aliases get,jobjRef,character-method
setGeneric(name = "get", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("get")
           })

## @include
#' @export
#' @rdname get-methods
#' @aliases get,ContractType,character-method
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngineModel,character-method
#' @aliases get,jobjRef,character-method
setMethod(f = "get", signature = c("ContractModel","character"),
          definition = function(object, what){
            
            ## what are the available terms?
            all <- terms(object)
            # return all if no specific term requested
            if (missing(what)) {
              what <- "all"
            }
            ## what are the terms we want return?
            if (what[1] == "all") {
              what <- all
            }
            ## which terms are available?
            match <- what[what %in% all]
            ## create instance of return object
            out <- list()
            ## get the available terms
            for (i in 1:length(match)) {
              out[[match[i]]] <- object$ContractTerms[[match[i]]]
            }
            ## are there any terms we could not find?
            nomatch <- what[!what %in% all]
            for (i in nomatch) {
              warning(paste("ErrorIn::ContractModel:: Could not find term ", i, "!"))
            }
            return(out)
          })


setGeneric(name = "checkArguments", useAsDefault = TRUE,
           def = function(object, arguments, ...){
             standardGeneric("checkArguments")
           })

setMethod(f = "checkArguments", signature = c("ContractModel","list"),
          definition = function(object, arguments, ...){
            
            # check if the attribute names are all existing
            test_attr <- names(arguments) %in% names(object$allowed)
            if (sum(as.numeric(test_attr)) < length(test_attr)) {
              stop(paste("ErrorIn::ContractModel:: Attributes called '", names(arguments)[!test_attr], "' are not allowed !!!"))
            }
            
            # check if the given inputs are valid...
            for (i in 1:length(arguments)) {
              data_type <- object$allowed[[names(arguments[i])]]
              if (!is.null(data_type)) {
                if (data_type[1] == "ISO8601 Datetime") {
                  tryCatch(as.Date(arguments[[i]]),
                           error = function(e) {
                             stop(paste("ErrorIn::ContractModel:: A Value of '", arguments[[i]], "' is not allowed for Attribute '",names(arguments[i]),". Must be 'ISO8601 Datetime' !!!"))
                           })
                } else if (data_type[1] == "ISO4217") {
                  if (arguments[[i]] != toupper(arguments[[i]]) | nchar(arguments[[i]]) != 3) {
                    stop(paste("ErrorIn::ContractModel:: A Value of '", arguments[[i]], "' is not allowed for Attribute '",names(arguments[i]),". Must be 'ISO4217' !!!"))
                  }
                } else if (data_type[1] == "Positive") {
                  if (arguments[[i]] < 0) {
                    stop(paste("ErrorIn::ContractModel:: A Value of '", arguments[[i]], "' is not allowed for Attribute '",names(arguments[i]),". Must be Positive !!!"))
                  }
                } else if (data_type[1] == "Negative") {
                  if (arguments[[i]] > 0) {
                    stop(paste("ErrorIn::ContractModel:: A Value of '", arguments[[i]], "' is not allowed for Attribute '",names(arguments[i]),". Must be Negative  !!!"))
                  }
                } else {
                  if (grepl(",", arguments[[i]], fixed = TRUE)) {
                    arg_vec <- unlist(strsplit(arguments[[i]], ", "))
                    if (sum(arg_vec %in% data_type) == 0) {
                      stop(paste("ErrorIn::ContractModel:: Some of the values '", arguments[[i]], "' are not allowed for Attribute '",names(arguments[i]),"' !!!"))
                    }
                  } else {
                    if (!arguments[[i]] %in% data_type) {
                      stop(paste("ErrorIn::ContractModel:: A Value of '", arguments[[i]], "' is not allowed for Attribute '",names(arguments[i]),"' !!!"))
                    }
                  }
                }
              }
            }
          })

##############################################################
#' Generic method to set the value of term(s) or parameter(s) 
#' of an ACTUS object
#'
#' Implemented e.g. for \code{\link{ContractModel}} where 
#' \code{set} populates the value for specified contract 
#' term(s) or for \code{\link{RiskFactor}} implementations, 
#' \code{\link{ValuationModel}}, and whatever object of this 
#' package that is carrying parameters.
#' Note that argument \code{what} has to refer to the exact
#' names of the terms or parameters to populate. The values
#' need to be of the respective data type, i.e. numeric,
#' character, logical, etc. depending on the term to be set.
#' 
#' @param object The object for which to set the term value(s)
#' 
#' @param what A list consisting of name/value elements where
#'        an element's name refers to the name of the term to
#'        populate and value provides the respective value.
#'        
#' @param ...
#'
#' @return
#' 
#' @seealso \code{\link{terms,get}}
#'
#' @examples
#' pam = Pam()
#' terms(pam) # get all term names
#' set(pam, what=list(NotionalPrincipal=1000,
#'                    DayCountConvention="A/AISDA",
#'                    InitialExchangeDate="2015-01-01T00"))
#' get(pam, what=c("NotionalPrincipal", "DayCountConvention", 
#'                    "InitialExchangeDate"))
#' 
## @include
#' @export
#' @docType methods
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
#' @aliases set,jobjRef,list-method
setGeneric(name = "set", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("set")
           })

## @include
#' @export
setMethod("show", signature = "ContractModel",
          definition = function(object){
            tempList <- get(object = object, what = "all")
            print(tempList)
          })

