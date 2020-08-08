#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
#' @include MaturityContract.R
#' @export
#' @rdname ct-classes
setRefClass("PrincipalAtMaturity",
            contains = "ContractType")

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "Pam",
           def = function(...){
             standardGeneric("Pam")
           })
## @include 
#' @export
#' @rdname ct-methods
setMethod(f = "Pam", signature = c(),
          definition = function(
            ContractID = "001",
            ContractRole = "RPA",
            StatusDate = "0000-01-01",
            PremiumDiscountAtIED = 0,
            DayCountConvention = "30E/360",
            ...){
              object <- new("PrincipalAtMaturity")
              pars <- list(...,
                           ContractID = ContractID,
                           ContractRole = ContractRole,
                           StatusDate = StatusDate,
                           PremiumDiscountAtIED = PremiumDiscountAtIED,
                           DayCountConvention = DayCountConvention)
              pars$ContractType = "PAM"
              if (is.list(pars[[1]])) {
                stop("ErrorIn::PrincipalAtMaturity:: List as input deprecated !!!")
              }
              if (is.null(pars$ContractDealDate)) {
                pars$ContractDealDate = pars$InitialExchangeDate
              }
              set(object = object, what = pars)
              return(object)
          })

## @include
#' @export
#' @rdname ct-methods
setGeneric(name = "PrincipalAtMaturity",
           def = function(...){
             standardGeneric("PrincipalAtMaturity")
           })
## @include 
#' @export
#' @rdname ct-methods
setMethod(f = "PrincipalAtMaturity", signature = c(),
          definition = function(...){
            object <- Pam(...)
            return(object)
          })
## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
setMethod(f = "initialize", signature = "PrincipalAtMaturity",
          function(.Object, ...) {
              .Object <- callNextMethod()
              atts <- lapply(CTM("PrincipalAtMaturity")$attributes, function(x) {"NULL"})
              .Object$attributes <- atts
              return(.Object)
          })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "PrincipalAtMaturity",
#           function(object){
#             print(get(object = object, what = get.summary.fields()))
#           })
