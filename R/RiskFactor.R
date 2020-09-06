#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that represents all Risk Factor objects
#' 
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to 
#' serve as parent to various implementations of Risk Factor 
#' types as a means for designing method-inheritage. 
#' 
#' @field MarketObjectCode character name of the risk factor 
#' @field Data data.frame representating time series data
#' 
#' @seealso \code{\link{YieldCurve, ReferenceIndex, ForeignExchangeRate}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname rf-classes
setRefClass("RiskFactor",
            fields = list(MarketObjectCode = "character",
                          Data = "timeSeries"
            ))
##############################################################
#' \code{RiskFactor}-class constructor
#'
#' Create an instance of an implementation of class 
#' \code{RiskFactor} (e.g. \code{\link{YieldCurve}},
#' \code{\link{ReferenceIndex}}, etc). 
#' This constructor is in fact a short cut to the constructors
#' of the implemented classes such as \code{\link{YieldCurve}} for 
#' class \code{\link{YieldCurve}}, \code{\link{Index}} for 
#' class \code{\link{ReferenceIndex}} or \code{\link{FxRate}} for 
#' class \code{\link{ForeignExchangeRate}}. Note that it is not possible to
#' instanciate class \code{RiskFactor} itself but only the
#' implementing classes extending \code{RiskFactor}.
#' 
#' @param object character, where \code{object} is expected to be the 
#'        R-class name of the RiskFactor-implementation to be instantiated.
#'
#' @return An object of a class extending \code{RiskFactor} 
#' 
#' @seealso \code{\link{YieldCurve, ReferenceIndex}}
#'
#' @examples 
#' # create a new 'YieldCurve' object
#' yc = RF("YieldCurve")
#'
## @include
#' @export
#' @docType methods
#' @rdname rf-methods
setGeneric(name = "RF",
           def = function(object){
             standardGeneric("RF")
           })

## @include
#' @export
#' @rdname rf-methods
setMethod(f = "RF", signature = c("character"),
          definition = function(object) {
            return(new(object))
          })
