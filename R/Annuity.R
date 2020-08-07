#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
#' @include MaturityContract.R 
#' @export
#' @rdname ct-classes
setRefClass("Annuity",
            contains = c("MaturityContract"),
            fields = list(
            ))


## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Ann",
           def = function(...){
             standardGeneric("Ann")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(
  f = "Ann",
  signature = c(),
  definition = function(ContractID = "001",
                        ContractRole = "RPA",
                        StatusDate = "0000-01-01T00",
                        PremiumDiscountAtIED = 0,
                        DayCountConvention = "30E/360",
                        ...) {
    
    pars <- list(...,
                 ContractID = ContractID,
                 ContractRole = ContractRole,
                 StatusDate = StatusDate,
                 PremiumDiscountAtIED = PremiumDiscountAtIED,
                 DayCountConvention = DayCountConvention )
    if (is.null(pars$ContractDealDate)) {
      pars$ContractDealDate = pars$InitialExchangeDate
    }
    pars$ContractType = "ANN"
    object <- new("Annuity")
    set(object = object, what = pars)
    return(object)
  }
)

# setMethod(f = "Ann",signature = c(),
#           definition = function(
#             ContractID = "001",
#             ContractRole = "RPA",
#             StatusDate = "0000-01-01T00",
#             PremiumDiscountAtIED = 0,
#             DayCountConvention = "30E/360",
#             ...){
#               object = new("Annuity")
#               pars <- list(...,
#                            ContractID = ContractID, 
#                            ContractRole = ContractRole, 
#                            StatusDate = StatusDate,
#                            PremiumDiscountAtIED = PremiumDiscountAtIED,
#                            DayCountConvention = DayCountConvention,
#                            ContractType = "ANN"
#               )
#               if (length(pars) == 0) {
#               }else if (is.list(pars[[1]])) {
#                   pars <- pars[[1]]
#               }else{
#                   if (is.null(pars$ContractDealDate)) {
#                     pars$ContractDealDate = pars$InitialExchangeDate
#                   }
#               }
#               pars <- date.conversion.attributes(pars)
#               set(object = object, what = pars)
#               return(object)
#           })

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Annuity",
           def = function(...){
             standardGeneric("Annuity")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "Annuity",signature = c(),
          definition = function(...){
            object <- Ann(...)
            return(object)
          })


## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "Annuity",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("Annuity")$attributes, function(x) {"NULL"})
              .Object$attributes <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
setMethod(f = "summary", signature = "Annuity",
          function(object){
              print(get(object = object, what = get.summary.fields("Ann")))
          })

# a=ActusDictionary$rflActus_attributes[["Annuity"]]
# ann
# $ContractID
# [1] "001"
# $ContractRole
# [1] "RPA"
# $StatusDate
# [1] "0000-01-01T00"
# $PremiumDiscountAtIED
# [1] 0
# $DayCountConvention
# [1] "30E/360"
# $ContractType
# [1] "ANN"
# a$ContractID
# names(a)
# a$ContractID
# ann=Ann()
# getModelDetails(ann)