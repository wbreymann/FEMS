#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include MaturityContract.R
#' @export
#' @rdname ct-classes
setRefClass("ExoticLinearAmortizer",
            contains = "MaturityContract",
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Lax",
           def = function(...){
             standardGeneric("Lax")
           })

## @include
#' @export
#' @rdname ct-methods
#' 
setMethod(
  f = "Lax",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType = "LAX"
    object <- new("ExoticLinearAmortizer")
    set(object = object, what = pars)
    return(object)
  }
)

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "ExoticLinearAmortizer",
           def = function(...){
             standardGeneric("ExoticLinearAmortizer")
           })


## @include 
#' @export
#' @rdname ct-methods
setMethod(f = "ExoticLinearAmortizer", signature = c(),
          definition = function(...){
            object <- Lax(...)
            return(object)
          })


## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "ExoticLinearAmortizer",
          function(.Object, ...) {
            .Object <- callNextMethod()
            atts <- lapply(CTM("ExoticLinearAmortizer")$ContractTerms, function(x) {"NULL"})
            .Object$ContractTerms <- atts
            return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "ExoticLinearAmortizer",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Lax")))
#           })

# names(ActusDictionary$rflActus_attributes$ExoticLinearAmortizer$ContractType)
# names(ActusDictionary$rflActus_attributes$LinearAmortizer$ContractType)
# # lax_example=Lax()
# # getModelDetails(lax_example)
# ActusDictionary$rflActus_allowed_vals$Swap$ContractType
#lax_example=Lax()
#getModelDetails(lax_example)