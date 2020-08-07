#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
 
##############################################################
#' Derive the accrued interest of a contract per specific time(s)
#'
#' 
#'
#' @param object object The \code{ContractType} or \code{EventSeries}-object for which to derive the value
#'
#' @param by time as per which to compute accrued interest (can be a vector)
#'
#' @param model (optional) The \code{RiskFactorConnector} conditional to which events are computed
#'
#' @return A \code{numeric} object representing the accrued interest per time
#' 
#' @seealso \link{ContractType}, \link{EventSeries}, \link{RiskFactorConnector}
#'
#' @examples
#' # define contract
#' pam = Pam(what=list(
#'                     ContractID = "001",
#'                     Currency = "USD",
#'                     ContractDealDate = "2012-12-31T00",
#'                     InitialExchangeDate = "2013-01-01T00",
#'                     MaturityDate = "2017-12-31T00",
#'                     StatusDate       = "2012-12-31T00",
#'                     NotionalPrincipal = 1000,
#'                     NominalInterestRate = 0.04,
#'                     CycleOfInterestPayment = "1Y-",
#'                     ContractRole = "RPA",
#'                     DayCountConvention = "30E/360"
#' ))
#' 
#' # time-vector
#' by=timeSequence(from=substring(get(pam, "InitialExchangeDate"), 1, 10),
#'                 to=substring(get(pam,"MaturityDate"), 1, 10),
#'                 by="1 months")
#'
#' # compute accrued interest for time-vector
#' accrued(pam, by)
#' 
## @include 
#' @export
#' @docType methods
#' @rdname accrued-methods
setGeneric(name = "accrued", def = function(object, by, model, ...){
  standardGeneric("accrued")
})

#' @include ContractType.R
#' @export
#' @rdname accrued-methods
setMethod(f = "accrued", signature = c("ContractType", "timeDate","missing"),
          definition = function(object, by, model){
  return(accrued(object,as.character(by)))          
})

#' @include ContractType.R
#' @export
#' @rdname accrued-methods
setMethod(f = "accrued", signature = c("ContractType", "character", "missing"),
          definition = function(object, by, model){
  out=sapply(by,function(t) get(events(object,t),"evs")$NominalAccrued[1])
  return(out)
})

#' @include ContractType.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname accrued-methods
setMethod(f = "accrued", signature = c("ContractType", "timeDate","RiskFactorConnector"),
          definition = function(object, by, model){
            return(accrued(object,as.character(by),model))          
          })

#' @include ContractType.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname accrued-methods
setMethod(f = "accrued", signature = c("ContractType", "character", "RiskFactorConnector"),
          definition = function(object, by, model){
  out=sapply(by,function(t) get(events(object,t,model),"evs")$NominalAccrued[1])
  return(out)
})


##################################################################33o
# pam = Pam(what=list(
#   ContractID = "001",
#   Currency = "USD",
#   ContractDealDate = "2012-12-31T00",
#   InitialExchangeDate = "2013-01-01T00",
#   MaturityDate = "2017-12-31T00",
#   StatusDate       = "2012-12-31T00",
#   NotionalPrincipal = 1000,
#   NominalInterestRate = 0.04,
#   CycleOfInterestPayment = "1Y-",
#   ContractRole = "RPA",
#   DayCountConvention = "30E/360"
# ))
# #
# # # time-vector
# by=timeSequence(from=substring(get(pam, "InitialExchangeDate"), 1, 10),
#                 to=substring(get(pam,"MaturityDate"), 1, 10),
#                 by="1 months")
# #
# # # compute accrued interest for time-vector
# accrued(pam, by)
# object=pam
# t=as.character(by)[2]
# get(events(pam,t),"evs")$NominalAccrued[1]
# 
# a=events(pam,"2013-01-01")
# get(a,"evs")$NominalAccrued