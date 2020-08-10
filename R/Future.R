#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @include CompoundContract.R
#' @export
#' @rdname ct-classes
setRefClass("Future",
            contains = c("CompoundContract"),
            fields = list(
            ))

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Futur",
           def = function(...){
             standardGeneric("Futur")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Futur",
  signature = c(),
  definition = function(...) {
    pars <- list(...)$what
    pars$ContractType = "FUTUR"
    object <- new("Future")
    set(object = object, what = pars)
    return(object)
  }
)
# setMethod(f = "Futur", signature = c(),
#           definition = function(...){
#               fut = new("Future")
#               pars = list(...,
#                            ContractType = "FUTUR"
#                           )
#               if(length(pars)==0){
#               }else if(is.list(pars[[1]])){
#                   set(object=fut, what=pars[[1]])
#               }else{
#                   set(object=fut, what=pars)
#               }
#               return(fut)
#           })
## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "Future",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("Future")$ContractTerms, function(x) {"NULL"})
              .Object$ContractTerms <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "Future",
#           function(object){
#             print(get(object = object, what = get.summary.fields("Futur")))
#           })

# futur_example=Futur()
# getModelDetails(futur_example)