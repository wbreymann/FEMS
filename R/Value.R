#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the value at AD0 of a contract
#'
#' Different value-concepts can be derived from a financial instrument
#' or the resulting EventSeries, respectively. Currentently, these are
#' Nominal value and Mark-to-Model value.
#'
#' Nominal value is the value of a Maturity instrument that is currently 
#' outstanding as a dept from the counterparty on the liability side to
#' the party on the asset side. The payment pattern of the nominal value
#' over time is what distincts different Maturity instrument types from
#' each other.
#' Actus CT algorithms generate the nominal value over time together with
#' cash flows. Hence, the nominal value for a certain analysis date may 
#' only be derived from the first-level results of a Maturity contract.
#'
#' Mark-to-model value is the value of a financial instrument according
#' to a certain valuation model. E.g. for Maturity instruments, this is usually
#' its net-present-value. Mostly, this value is used if no value can be observed 
#' for this instrument on the market. This can be the case if the market for this
#' instrument is illiquid. 
#' In order for a mark-to-model value to be derived, the CT algorithm must have
#' been processed and an appropriate \code{ValuationEngine} either assigned in 
#' advance or past as method argument 'method'.
#'
#' @param object object The \code{ContractType} or \code{EventSeries}-object for which to derive the value
#'
#' @param by time as per which to compute the value
#'
#' @param type A character representing the type of value (either 'nominal' or 'market')
#' 
#' @param method (optional) for type='market' a 'ValuationEngine' can be specified according to which the value is computed
#' 
#' @param ... Currently unused
#' 
#' @return A \code{numeric} object representing the contract's value
#' 
#' @seealso \code{\link{ContractType}} and \code{\link{ValuationEngine}}
#'
#' @examples
#' pam <- Pam()
#' set(pam, what=list(
#'                  ContractID = "001",
#'                  Currency = "CHF",
#'                  Calendar = "Weekday",
#'                  ContractRole = "RPA",               
#'                  StatusDate       = "2016-05-30T00",
#'                  ContractDealDate = "2016-05-30T00",
#'                  InitialExchangeDate = "2016-05-30T00",
#'                  MaturityDate = "2016-09-01T00",
#'                  NotionalPrincipal = 1000,
#'                  NominalInterestRate = 0.00, 
#'                  PremiumDiscountAtIED = -100,
#'                  DayCountConvention = "30E/360",
#'                  BusinessDayConvention = "SCF"))
#' ad <- "2016-06-01T00"
#' yc <- YieldCurve()
#' set(yc, what=list(Nodes=list(ReferenceDate=ad,
#'                              Tenors=c("1M","10Y"),
#'                              Rates=c(0.005,0.02)),
#'                   MarketObjectCode = "RiskFreeCurve"))
#' rf <- RFConn()
#' add(rf,yc)
#' set(pam,rf)
#' 
#' # derive nominal value
#' value(pam,by=ad,type="nominal")
#' 
#' # define valuation engine for mark-to-model value
#' dcEngine <- DcEngine()
#' set(dcEngine,list(RiskFactorObjectLink="RiskFreeCurve",
#'                   DiscountingSpread=0.0))
#' set(dcEngine,rf)
#' set(pam,dcEngine)
#' value(pam,by=ad,type="market")
#' 
#' # again with a different engine
#' set(dcEngine,list(RiskFactorObjectLink="RiskFreeCurve",
#'                   DiscountingSpread=0.1))
#' value(pam,by=ad,type="market",method=dcEngine)
#' 
#' @include EventSeries.R Portfolio.R DiscountingEngine.R
#' @export
#' @docType methods
#' @rdname val-methods
setGeneric(name = "value", def = function(object, by, type, method, ...){
  standardGeneric("value")
})

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "character", "character", "missing"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            if (type == "nominal") {
              val <- FEMS::value(events(object, by[1]), by , "nominal", ...) 
            } else if (type %in% c("market")) {
              stop("Please specify argument 'method' when computing 'market' for an EventSeries!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(val)
          })

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "AD0", "character", "missing"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            if (type == "nominal") {
              # AD0 has only a single element
              val <- FEMS::value(FEMS::events(object, by), as.character(by), type, ...)
            } else if (type %in% c("market")) {  
              stop("Please specify argument 'method' when computing 'market' for an EventSeries!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(val)
          })

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeDate", "character", "missing"),
          definition = function(object, by, type, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeBuckets", "character", "missing"),
          definition = function(object, by, type, ...){
            val <- FEMS::value(object, as.character(by), type, ...)
            names(val) <- by@breakLabs
            return(val)
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeBuckets", "character", "YieldCurve"),
          definition = function(object, by, type, method, spread = 0, ...){
            type <- temp.func.type.deprecated(type)
            eng <- DcEngine()
            set(eng, what = list(DiscountingSpread = spread,
                                 RiskFactorObject = method))
            val <- FEMS::value(object, as.character(by), type, method=eng)
            names(val) <- by@breakLabs
            return(val)
          })


#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "nominal", method, ...)
          } else if (type %in% c("market") ) {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "market", method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeDate", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, method, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeBuckets", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, method, ...)
            names(val) <- by@breakLabs
            return(val)
          })

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "AD0", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object,ad), as.character(by), type, ...)
            } else if (type %in% c("market")) {
              val <- FEMS::value(FEMS::events(object, by), as.character(by), type, method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            } 
            return(val)
          })
          
#################################################################
##### Various methods with object of class EventSeries as Input...
#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "AD0", "character", "missing"),
          definition = function(object, by, type, method, ...) {
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, ...))        
          })



#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeDate", "character", "missing"),
          definition = function(object, by, type, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, ...) )
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeBuckets", "character", "missing"),
          definition = function(object, by, type, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, ...)
            names(val) <- by@breakLabs
            return(val)
          })


#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeDate", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, method, ...) )
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeBuckets", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, method, ...)
            names(val) <- by@breakLabs
            return(val)
          })

#############################################################################################
# all value functionality related to eventLists

#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "timeDate", "character", "missing"), 
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, ...))
          })

## @include 
#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "timeBuckets", "character", "missing"), 
          definition = function(object, by, type, ...){
            type <- temp.func.type.deprecated(type)
            val = FEMS::value(object, as.character(by), type, ...)
            names(val) = by@breakLabs
            return(val)
          })


#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "timeDate", "character", "DiscountingEngine"), 
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            return(FEMS::value(object, as.character(by), type, method, ...))
          })

## @include 
#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "timeBuckets", "character", "DiscountingEngine"), 
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val = FEMS::value(object, as.character(by), type, method, ...)
            names(val) = by@breakLabs
            return(val)
          })


#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "character", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, digits = 2, ...){
            type <- temp.func.type.deprecated(type)
            if(type=="nominal") {
              val = FEMS::value(object, by, "nominal", digits=digits, ...)
            } else if (type %in% c("market")) {
              # add spread to interest rate
              spread <- FEMS::get(method, "DiscountingSpread")
              dc <- FEMS::get(method, "RiskFactorObject")
              FEMS::set(dc, list(Rates=FEMS::get(dc, "Rates") + spread))
              val = sapply(by, function(ad) { # loop over elements in "by"
                evs = data.frame(
                  FEMS::get(object, "evs")$Date,
                  FEMS::get(object, "evs")$Value,
                  FEMS::get(object, "evs")$Type
                )
                colnames(evs) = c("times", "values", "types")
                evs$times = timeDate(evs$times)
                # In the next line: it should be a ">", not ">="
                # Otherwise inconsistent with result from same method
                # computed from Contract
                evs.sub = subset(evs, times >= timeDate(ad))
                evs.sub <- subset(evs.sub, !(evs.sub$types %in% c("DPR","IPCI")))
                if( nrow(evs.sub)==0 | sum(evs.sub$types %in% c("IED","PRD")) > 0
                    # | evs.sub["times", evs.sub$types=="IED"] > ad
                ) {
                  # added condition that npv is 0 if ad is earlier than IED
                  # (execution of contract has not yet started)
                  return(0.0)
                } else {
                  cfs = evs.sub$values
                  dts = as.character(evs.sub$times)
                  dfs = FEMS::discountFactors(dc, from=ad, to=dts)
                  return( as.numeric (cfs%*%dfs ))
                }
              })
              # rebase yield curve
              set(dc, list(Rates=get(dc, "Rates")-spread)) # TODO: implement discounting more consistently
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(round(val, digits))
          
          })


#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "character", "character", "missing"),
          definition = function(object, by, type, digits = 2, ...){
            type <- temp.func.type.deprecated(type)
            if(type=="nominal") {
              val = sapply(by, function(ad) {
                evs = data.frame(
                  FEMS::get(object,"evs")$Date,
                  FEMS::get(object,"evs")$NominalValue,
                  FEMS::get(object,"evs")$Type
                )
                colnames(evs) = c("times","values","types")
                evs$times = timeDate(evs$times)
                evs.sub = subset(evs,times<=timeDate(substring(ad,1,10)))
                if (dim(evs.sub)[1] == 0) {
                  evs.sub = data.frame(times=ad, values=0,types="AD0")
                }
                evs.last = evs.sub[which(evs.sub$times==max(evs.sub$times)),]
                return(evs.last$values[nrow(evs.last)])
              })
            } else if( type %in% c("market") ) {
              stop("Please specify argument 'method' when computing 'market' for an EventSeries!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(round(val, digits))
          })



#' @include EventSeries.R
#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "character", "character", "missing"), 
          definition = function(object, by, type,  ...){
            type <- temp.func.type.deprecated(type)
            # print("signature:")
            # print(c("eventList", "character", "character", "missing"))
            pars = list(...)
            if ("digits" %in% names(pars)) {
              digits = pars[["digits"]]
            } else {
              digits = 2
            }
            # print(paste0("digits = ", digits))
            if ("filter" %in% names(pars)) {
              tmp = object[names(object) %in% pars[["filter"]][[1]]]
              object = new("eventList")
              object[names(tmp)] = tmp
            }
            if (type=="nominal") {
              # out = t(sapply(object, FEMS::value, by = by, type = type, 
              #                digits = digits))
              out = t(sapply(object, FEMS::value, by = by, type = type, 
                             digits=digits))
              ids = names(object)
              out = matrix(out, ncol = length(by))
              colnames(out) = by
              rownames(out) = ids
              
              if ("tree" %in% names(pars)) {
                tree = pars[["tree"]]
                leafs = tree$leafs
                out = lapply(leafs, FUN = function(x) {
                  apply(matrix(out[x, ], ncol = length(by)), 2, sum)
                })
                out = FEMS:::aggregate.leafs(out, tree$branches, type)
                colnames(out) = by
              } else {
                out = apply(out, 2, sum)
              }
            } else if (type=="market") {
              stop("Please specify 'method' in order to compute 'market'-value for a list of 'EventSeries'-objects!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(out)
          })


#' @include EventSeries.R
#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "character", "character", "DiscountingEngine"), 
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            # print("signature:")
            # print(c("Portfolio", "character", "character", "ValuationEngine"))
            if (type=="nominal") {
              out = value(object, by, type, ...)
            } else if (type=="market") {
              pars = list(...)

              if ("digits" %in% names(pars)) {
                digits = pars$digits
              } else {
                digits = 2
              }

              if ("filter" %in% names(pars)) {
                tmp = object[names(object) %in% pars[["filter"]][[1]]]
                object = new("eventList")
                object[names(tmp)] = tmp
              }

              out = t(sapply(object, 
                             FEMS::value, by = by, type = type, method = method, 
                             digits = digits))
              ids = names(object)
              
              out = matrix(out, ncol = length(by))
              colnames(out) = by
              rownames(out) = ids
              
              if ("tree" %in% names(pars)) {
                tree = pars[["tree"]]
                leafs = tree$leafs
                out = lapply(leafs, FUN = function(x) {
                  apply(matrix(out[x, ], ncol = length(by)), 2, sum)
                })
                out = FEMS:::aggregate.leafs(out, tree$branches, type)
                colnames(out) = by
              } else {
                out = apply(out, 2, sum)
              }          

            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(out)
          })

#' @include EventSeries.R
#' @export
#' @docType methods
#' @rdname val-methods
setMethod("value", 
          signature = c("eventList", "character", "character", "list"), 
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            # print("signature:")
            # print(c("eventList", "character", "character", "list"))
            if (type=="nominal") {
              out = value(object, by, type, ...)
            } else if (type=="market") {
              pars = list(...)
              if ("digits" %in% names(pars)) {
                digits = pars$digits
              } else {
                digits = 2
              }
              # print(paste0("digits = ", digits))
              if ("filter" %in% names(pars)) {
                tmp = object[names(object) %in% pars[["filter"]][[1]]]
                object = new("eventList")
                object[names(tmp)] = tmp
              }
              out = t(sapply(1:length(object), function(i) {
                FEMS::value(object = object[[i]], by = by, type = type, 
                              method = method[[i]], digits = digits)
              }))
              out = matrix(out, ncol = length(by))
              colnames(out) = by
              rownames(out) = names(object)
              
              if ("tree" %in% names(pars)) {
                tree = pars[["tree"]]
                leafs = tree$leafs
                out = lapply(leafs, FUN = function(x) {
                  apply(matrix(out[x, ], ncol = length(by)), 2, sum)
                })
                out = FEMS:::aggregate.leafs(out, tree$branches, type)
                colnames(out) = by
              } else {
                out = apply(out, 2, sum)
              } 
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(out)
          })

#################################################################
##### Various methods with object of class Portfolio as Input...

setMethod(f = "value", signature = c("Portfolio", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "nominal", method, ...)
            } else if (type %in% c("market") ) {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "market", method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "character", "character", "missing"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(FEMS::events(object, by[1]), by, type, ...)
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeBuckets", "character", "missing"),
          definition = function(object, by, type, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, ...)
            names(val) <- by@breakLabs
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeBuckets", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, method, ...)
            names(val) <- by@breakLabs
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeDate", "character", "missing"),
          definition = function(object, by, type, ...){
            val <- FEMS::value(object, as.character(by), type, ...)
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeDate", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            type <- temp.func.type.deprecated(type)
            val <- FEMS::value(object, as.character(by), type, method, ...)
            return(val)
          })


check.start.date <- function(start_dt, start_dt_evs) {
  if (start_dt < start_dt_evs){
    stop("ErrorIn::Value:: Value calculation cannot be performed before first event (InitialExchangeDate) !!! ")
  }
}

temp.func.type.deprecated <- function(type) {
  if (type %in% c("markToModel", "markToMarket")) {
    cat("The type 'markToModel' or 'markToMarket' will be deprecated in future releases.\n")
    cat("Please use 'market' instead.\n")
    type <- "market"
  }
  return(type)
}
