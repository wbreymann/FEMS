#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************


#' @include MaturityContract.R
#' @export
#' @rdname ct-classes
setRefClass("LinearAmortizer",
            contains = "MaturityContract",
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Lam",
           def = function(...){
             standardGeneric("Lam")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Lam",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType <- "LAM"
    object <- new("LinearAmortizer")
    set(object = object, what = pars)
    return(object)
  }
)
# setMethod(f = "Lam",signature = c(),
#           definition = function(...){
#               object = new("LinearAmortizer")
#               pars = list(...,
#                            ContractType = "LAM"
#                           )
#               if(length(pars)==0){
#               }else if(is.list(pars[[1]])){
#                   set(object=object, what=pars[[1]])
#               }else{
#                   set(object=object, what=pars)
#               }
#               return(object)
#           })


## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "LinearAmortizer",
           def = function(...){
             standardGeneric("LinearAmortizer")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "LinearAmortizer",signature = c(),
          definition = function(...){
            object <- Lam(...)
            return(object)
          })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
#' @export
## @rdname
setMethod(f = "initialize", signature = "LinearAmortizer",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("LinearAmortizer")$attributes, function(x) {"NULL"})
              .Object$attributes <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "LinearAmortizer",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Lam")))
#           })

# lam_example=Lam()
# getModelDetails(lam_example)