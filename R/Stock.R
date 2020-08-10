#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include CompoundContract.R
#' @export
#' @rdname ct-classes
setRefClass("Stock",
         contains = c("ContractType"),
         fields = list(
         ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Stk",
           def = function(...){
             standardGeneric("Stk")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Stk",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType <- "STK"
    object <- new("Stock")
    set(object = object, what = pars)
    return(object)
  }
)

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Stock",
           def = function(...){
             standardGeneric("Stock")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "Stock",signature = c(),
          definition = function(...){
            object <- Stk(...)
            return(object)
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "Stock",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("Stock")$ContractTerms, function(x) {"NULL"})
              .Object$ContractTerms <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "Stock",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Stk")))
#           })

#object$allowed$ContractType
# [1] "PAM"   "ANN"   "NAM"   "LAM"   "LAX"   "CLM"   "UMP"   "CSH"   "STK"   "COM"   "SWAPS" "SWPPV" "FXOUT" "CAPFL"
# [15] "FUTUR" "OPTNS" "CEG"   "CEC"  
# stock_example=Stk()
# getModelDetails(stock_example)