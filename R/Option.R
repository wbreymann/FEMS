#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include CompoundContract.R
#' @export
#' @rdname ct-classes
setRefClass("Option",
            contains = c("CompoundContract"),
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Optns",
           def = function(...){
             standardGeneric("Optns")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Optns",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType <- "OPTNS"
    object <- new("Option")
    set(object = object, what = pars)
    return(object)
  }
)

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Option",
           def = function(...){
             standardGeneric("Option")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "Option",signature = c(),
          definition = function(...){
            object <- Optns(...)
            return(object)
          })


## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "Option",
          function(.Object, ...) {
              .Object <- callNextMethod()
              .Object$attributes <- CTM("Option")$attributes
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
setMethod(f = "summary", signature = "Option",
          function(object){
            print(get(object = object, what = get.summary.fields("Optns")))
          })


# option_example=Optns()
# getModelDetails(futur_example)