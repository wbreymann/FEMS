#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
#' @include MaturityContract.R
#' @export
#' @rdname ct-classes
setRefClass("NegativeAmortizer",
            contains = "MaturityContract",
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Nam",
           def = function(...){
             standardGeneric("Nam")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Nam",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType = "NAM"
    object <- new("NegativeAmortizer")
    set(object = object, what = pars)
    return(object)
  }
)

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "NegativeAmortizer",
           def = function(...){
             standardGeneric("NegativeAmortizer")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "NegativeAmortizer",signature = c(),
          definition = function(...){
            object <- Nam(...)
            return(object)
          })


## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "NegativeAmortizer",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("NegativeAmortizer")$ContractTerms, function(x) {"NULL"})
              .Object$ContractTerms <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "NegativeAmortizer",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Nam")))
#           })

# nam_example=Nam()
# getModelDetails(nam_example)
# nam_example$ContractTerms$ContractType
# "ContractType" %in% names(nam_example$ContractTerms)