#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include CompoundContract.R
#' @export
#' @rdname ct-classes
setRefClass("ForeignExchangeOutright",
         contains = c("ContractType"),
         fields = list(
         ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Fxout",
           def = function(...){
             standardGeneric("Fxout")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Fxout",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType <- "FXOUT"
    object <- new("ForeignExchangeOutright")
    set(object = object, what = pars)
    return(object)
  }
)

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "ForeignExchangeOutright",
           def = function(...){
             standardGeneric("ForeignExchangeOutright")
           })


## @include 
#' @export
#' @rdname ct-methods
setMethod(f = "ForeignExchangeOutright", signature = c(),
          definition = function(...){
            object <- Fxout(...)
            return(object)
          })


## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature="ForeignExchangeOutright",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("ForeignExchangeOutright")$attributes, function(x) {"NULL"})
              .Object$attributes <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "ForeignExchangeOutright",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Fxout")))
#           })

# fxout_example=Fxout()
# getModelDetails(fxout_example)