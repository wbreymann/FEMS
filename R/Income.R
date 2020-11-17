#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the income-vector for \code{ContractType}
#' 
#' Income of a contract is computed over a (number of) specific time
#' interval(s) defined by argument \code{by}. Within such a time interval,
#' income is composed of two components: (1) nominal income as the
#' net payments from interest and fees, and (2) income from re-valuation
#' of the contract at beginning and end of the time interval. If at the 
#' begining of the time interval the contract's 'mark-to-model" value is
#' higher than at the end a re-valuation gain results and vice versa.
#' Thereby, the valuation model may be defined for each contract prior
#' to calling this function or specified using the function parameter 
#' 'method'.
#' 
#' Different income-concepts can be derived for a financial instrument
#' or the resulting EventSeries, respectively. Currentently, these are
#' Marginal income and Cumulative income.
#'
#' Marginal income-vector represents the aggregate income from interest
#' and fee payments within a
#' set of user-defined time-intervals. The time-intervals are defined as
#' a sequence of timeDate-dates. Thereby, the marginal income-vector
#' gives the net interest+fee cash flows within the specified 
#' time-intervals.
#'
#' Cumulative income-vector is the cumulative sum over time (-intervals)
#' of the marginal income-vector.
#'
#' @param object The \code{ContractType} or \code{EventSeries}-object for which to derive the income-vector
#'
#' @param by A sequence of 'timeDate's providing the target time-axis for the income-vector
#'
#' @param type A character representing the type of income (either 'marginal' or 'cumulative')
#' 
#' @param method A 'ValuationEngine' (or list thereof) giving the valuation methods used when computing the re-valuation gains as part of income
#'    
#' @param ... (optional) Use parameter 'revaluation.gains=FALSE' in order to return income solely from 
#' interest/fee payments
#'   
#' @return A \code{numeric} object representing the income-vector on the target time-axis
#' 
#' @seealso \code{\link{ContractType}} and \code{\link{EventSeries}}
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
#'                  MaturityDate = "2020-06-01T00",
#'                  NotionalPrincipal = 1000,
#'                  NominalInterestRate = 0.05,
#'                  CycleOfInterestPayment = "1Y-", 
#'                  PremiumDiscountAtIED = 0.0,
#'                  DayCountConvention = "30E/360",
#'                  BusinessDayConvention = "SCF"))
#' ad <- "2016-06-01T00"
#' 
#' # generate event series
#' evs=events(pam, ad)
#' 
#' # define target income time axis
#' by=timeSequence(substring(ad, 1, 10), "2020-06-01", by="1 year")
#' 
#' # derive marginal income from interest and fee payments for defined time axis
#' income(pam, by, "marginal", revaluation.gains=FALSE)
#' 
#' # derive cumulative income
#' income(pam, by, "cumulative", revaluation.gains=FALSE)
#' 
#' # now include revaluation gains
#' # therefore, define market environment and valuation method
#' yc=YieldCurve()
#' set(yc, what=list(Nodes=list(ReferenceDate=ad,
#'                              Tenors=c("1M", "10Y"),
#'                              Rates=c(0.005, 0.02)),
#'                   MarketObjectCode = "RiskFreeCurve"))
#' rf=RFConn()
#' add(rf, yc)
#' dcEngine <- DcEngine()
#' set(dcEngine, list(RiskFactorObjectLink="RiskFreeCurve",
#'                   dc.spread=0.0))
#' set(dcEngine, rf)
#' 
#' # now compute income with revaluation gains for defined time axis
#' income(pam, by, "marginal", dcEngine, revaluation.gains=TRUE)
#' income(pam, by, "cumulative", dcEngine, revaluation.gains=TRUE)
#' 
## @include 
#' @export
#' @docType methods
#' @rdname inc-methods
setGeneric(name = "income", 
           def = function(object, by, type, revaluation.gains, method, ...){
  standardGeneric("income")
})

#' @include ContractType.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "missing", "missing", "missing"),
          definition = function(object, by, ...){
            return(income(object, by, type="marginal", revaluation.gains=FALSE, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "missing", "missing", "missing"),
          definition = function(object, by, ...){
            inc = income(object, as.timeDate(by), type="marginal", revaluation.gains=FALSE, ...)
            names(inc) = by@bucketLabs
            return(inc)
          })

#' @include ContractType.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "character", "missing", "missing"),
          definition = function(object, by, type, ...){
            return(income(object, by=by, type, revaluation.gains=FALSE, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "character", "missing", "missing"),
          definition = function(object, by, type, ...){
            inc = income(object, by=as.timeDate(by), type, revaluation.gains=FALSE, ...)
            names(inc) = by@bucketLabs
            return(inc)
          })

#' @include ContractType.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "missing", "logical", "missing"),
          definition = function(object, by, revaluation.gains, ...){
            return(
              income(object, by=by, type="marginal", revaluation.gains=revaluation.gains, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "missing", "logical", "missing"),
          definition = function(object, by, revaluation.gains, ...){
            inc <- 
              income(object, by = as.timeDate(by), type = "marginal", 
                     revaluation.gains = revaluation.gains, ...)
            names(inc) <- by@bucketLabs
            return(inc)
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            if (revaluation.gains) {
              stop("If 'revaluation.gains==TRUE', the argument 'method' must be provided.")
            }
            if (type == "marginal") {
              events <- events(object,as.character(by[1]))
              inc <- income.from.payments(events, by, ...) + 
                income.from.accruals(object, by, ...)
            } else if (type == "cumulative") {
              inc <- cumsum(income(object, by, type="marginal", revaluation.gains=FALSE, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            inc = 
              income(object, by=as.timeDate(by), type=type, 
                     revaluation.gains=revaluation.gains, ...)
            names(inc) = by@bucketLabs
            return(inc)
          })


#' @include ContractType.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "missing", "logical", "ValuationEngine"),
          definition = function(object, by, revaluation.gains, method, ...){
            return(income(object, by=by, type="marginal", 
                          revaluation.gains=revaluation.gains, method=method, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "missing", "logical", "ValuationEngine"),
          definition = function(object, by, revaluation.gains, method, ...){
            inc = FEMS::income(object, by=as.timeDate(by), type="marginal", 
                          revaluation.gains=revaluation.gains, method=method, ...)
            names(inc) = by@bucketLabs
            return(inc)
          })


#' @include ContractType.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeDate", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            pars=list(...)
            # pars2 =  pars[names(pars)!="revaluation.gains"]
            if (type=="marginal") {
              events = events(object, as.character(by[1]))
              # if ("revaluation.gains" %in% names(pars) && 
              #     pars[["revaluation.gains"]]==FALSE) {
              if (!revaluation.gains) {
                  inc = income(object, by=by, type=type, revaluation.gains=FALSE, ...)
              } else {# note, income from accruals are already included in 
                      # income.from.revaluation since based on dirty price
                inc = income.from.payments(events, by, ...) + 
                  income.from.revaluation(object, by, method, ...)
              }
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", 
                                  revaluation.gains=revaluation.gains, method=method, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("ContractType", "timeBuckets", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            inc = income(object, by=as.timeDate(by), type=type, 
                         revaluation.gains=revaluation.gains, method=method, ...)
            names(inc) = by@bucketLabs
            return(inc)
          })

#' #' @include ContractType.R
#' #' @include ValuationEngine.R
#' #' @export
#' #' @rdname inc-methods
#' setMethod(f = "income", 
#'           signature = c("ContractType", "timeDate", "character", "ValuationEngine"),
#'           definition = function(object, by, type, method, ...){
#'             pars=list(...)
#'             # pars2 =  pars[names(pars)!="revaluation.gains"]
#'             if(type=="marginal") {
#'               events=events(object,as.character(by[1]))
#'               if("revaluation.gains" %in% names(pars) && pars[["revaluation.gains"]]==FALSE) {
#'               # if( !missing(revaluation.gains) && revaluation.gains==FALSE) {
#'                 inc = income.from.payments(events, by, ...) + 
#'                   income.from.accruals(object, by, ...)
#'               } else { # note, income from accruals are already included in 
#'                        # income.from.revaluation since based on dirty price
#'                 inc = income.from.payments(events, by, ...) + 
#'                   income.from.revaluation(object, by, method, ...)
#'               }
#'             } else if(type=="cumulative") {
#'               inc=cumsum(income(object, by, type="marginal", method, ...))
#'             } else {
#'               stop(paste("Income type '", type, "' not recognized!", sep=""))
#'             }
#'             return(inc)
#'           })



#################################################################################
############# ADDED Portfolio HERE 

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeDate", "missing", 
                                      "missing", "missing"),
          definition = function(object, by, type, ...){
            return(income(object, by, type="marginal", ...))
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeBuckets", "missing", 
                                      "missing", "missing"),
          definition = function(object, by, type, ...){
            inc = income(object, as.timeDate(by), type="marginal", ...)
            names(inc) = tb@bucketLabs
            return(inc)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeDate", "missing", 
                                      "missing", "ValuationEngine"),
          definition = function(object, by, method, ...){
            return(income(object, by, type="marginal", method, ...))
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeBuckets", "missing", 
                                      "missing", "ValuationEngine"),
          definition = function(object, by, method, ...){
            inc = income(object, as.timeDate(by), type="marginal", method, ...)
            names(inc) = tb@bucketLabs
            return(inc)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeDate", "character", 
                                      "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            
            pars <- list(...)
            if (!revaluation.gains) {
              inc <- income(events(object, as.character(by[1])), by, type, 
                           revaluation.gains = FALSE, ...)
            } else {
              ct.selected <- object$contracts
              names(ct.selected) <- FEMS:::get(object,"ids")
              if ("select" %in% names(pars)) {
                select.par <- unlist(lapply(
                  object$contracts, 
                  function(x) FEMS:::get(x, what=names(pars[["select"]]))
                ))
                ct.selected <- ct.selected[select.par %in% pars[["select"]][[1]]]
              }
              if ("tree" %in% names(pars)) {
                tree <- pars[["tree"]]
                leafs = lapply(tree$leafs, FUN=function(x) {
                  tmp = Portfolio()
                  tmp$contracts = ct.selected
                  tmp$contracts[!names(ct.selected) %in% x] = NULL
                  income(tmp, by = by, type = type, ...) 
                })
                inc <- aggregate.leafs(leafs, tree$branches, by)
              } else {
                inc <- apply(as.data.frame(lapply(ct.selected, income, by = by, 
                                               type = type, ... = ...)), 1, sum)
              }
            }
            return(inc)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeBuckets", "character", 
                                      "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            inc <- income(object, as.timeDate(by), type, revaluation.gains, ...)
            names(inc) <- tb@bucketLabs
            return(inc)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeDate", "character", 
                                      "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            pars=list(...)
            if(!revaluation.gains) {
              inc=income(events(object, paste0(by[1], "T00")), by, type, NULL, ...)
            } else {
              ct.selected = object$contracts
              names(ct.selected) = FEMS:::get(object, "ids")
              if("select" %in% names(pars)) {
                select.par = unlist(lapply(
                  object$contracts, 
                  function(x) FEMS:::get(x, what=names(pars[["select"]]))
                ))
                ct.selected = ct.selected[select.par %in% pars[["select"]][[1]]]
              }
              if("tree" %in% names(pars)) {
                tree = pars[["tree"]]
                leafs = lapply(tree$leafs, FUN=function(x) {
                  tmp = Portfolio()
                  # tmp$contracts = ct.selected
                  # tmp$contracts[!names(ct.selected) %in% x] = NULL
                  tmp$contracts = ct.selected[x]
                  # The following leads to an infinite loop because it goes 
                  # into the same selected case.
                  # income(tmp, by=by, type=type, revaluation.gains = TRUE,
                  #        method=method, ...)
                  # Replacement:
                  inc = apply(as.data.frame(lapply(
                    tmp$contracts, FUN=function(x) {
                      income (x, by=by, type=type, revaluation.gains=TRUE, 
                              method=method, ...)
                    })
                  ), 1, sum)
                  return(inc)
                })
                inc = FEMS:::aggregate.leafs(leafs, tree$branches, by)
              } else {
                inc = apply(as.data.frame(lapply(
                  ct.selected,
                  income,
                  by=by, type=type, revaluation.gains=TRUE, method=method, ...)
                ), 1, sum)
              }
            }
            # if ( is.null(dim(inc)) )
            #   inc = inc[-1]
            # else
            #   inc = inc[,-1]
            return(inc)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname inc-methods
setMethod(f = "income", signature = c("Portfolio", "timeBuckets", "character", 
                                      "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            inc = income(object, as.timeDate(by), type, revaluation.gains, 
                         method, ...)
            names(inc) = tb@bucketLabs
            return(inc)
          })


#####################################################################################




##------------------------------------------------------------------------------
#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "missing", "missing", "missing"),
          definition = function(object, by, ...) {
            return( income(object, by, type="marginal", ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "missing", "missing", "missing"),
          definition = function(object, by, ...) {
            return( income(object, as.timeDate(by), type="marginal", ...))
          })

#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "missing", "missing"),
          definition = function(object, by, type, ...){
            
            pars=list(...)
            # pars2 =  pars[names(pars)!="revaluation.gains"]
            if(type=="marginal") {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "missing", "missing"),
          definition = function(object, by, type, ...) {
            return( income(object, as.timeDate(by), type, ...))
          })

#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "missing", "logical", "missing"),
          definition = function(object, by, revaluation.gains, ...) {
            return( income(object, by, "marginal", revaluation.gains, ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "missing", "logical", "missing"),
          definition = function(object, by, revaluation.gains, ...) {
            return( income(object, as.timeDate(by), "marginal", revaluation.gains, ...))
          })

#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            
            pars=list(...)
            # pars2 =  pars[names(pars)!="revaluation.gains"]
            if(type=="marginal") {
              if( revaluation.gains ) {
                stop("If 'revaluation.gains=TRUE', 'method' must be specified.")
              } else {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
              }
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", revaluation.gains, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...) {
            return( income(object, as.timeDate(by), type, revaluation.gains, ...))
          })

#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "missing", "logical", "ValuationEngine"),
          definition = function(object, by, revaluation.gains, method, ...){
            inc = FEMS::income(object, by, "marginal", revaluation.gains, method, ...)
            return(inc)
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "missing", "logical", "ValuationEngine"),
          definition = function(object, by, revaluation.gains, method, ...) {
            return( income(object, as.timeDate(by), "marginal", revaluation.gains, method, ...))
          })


#' @include EventSeries.R
#' @include DiscountingEngine.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeDate", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...){
            pars=list(...)
            # pars2 =  pars[names(pars)!="revaluation.gains"]
            if(type=="marginal") {
              if( revaluation.gains ) {
                # stop("Cannot compute revaluation gains from an 'EventSeries' object! 
                #      But this is not correct!!!")
                inc = income.from.payments(object, by, ...) + 
                  income.from.revaluation.from.es(object, by, method, ...)
              } else {
                inc = income.from.payments(object, by, ...) + 
                  income.from.accruals.new(object, by, ...)
              }
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", revaluation.gains, 
                                  method, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", 
          signature = c("EventSeries", "timeBuckets", "character", "logical", "ValuationEngine"),
          definition = function(object, by, type, revaluation.gains, method, ...) {
            return( income(object, as.timeDate(by), type, revaluation.gains, method, ...))
          })

# internal util functions:
# -----------------------------------------------

# income from interest/fee
income.from.payments = function(eventSeries, by, digits=2, ...) {
  # Check that ICPI is income relevant!!
  income.events <- subset(as.data.frame(eventSeries), Type %in% c("IP","IPCI","FP","OPS","DPR","IED"))
  # adjust for PremiumDiscountsatIED
  idx.ied <- income.events$Type=="IED"
  income.events$Value[idx.ied] <- income.events$Value[idx.ied] + income.events$NominalValue [idx.ied]
  inc <- timeSeries(rep(0, length(by)), charvec = by)
  cf.raw <- timeSeries(income.events$Value, charvec = substring(income.events$Date, 1, 10))
  cf.aggr <- aggregate(cf.raw, by, FUN=sum)
  if (length(cf.aggr) > 0) {
    inc[time(cf.aggr),] <- cf.aggr
  }
  inc <- as.numeric(series(inc))[-1]
  return(round(inc, digits)) 
}

# nominal accrued
income.from.accruals = function(object, by, digits=2, ...) {
  return(round(c(as.numeric(diff(accrued(object, by)))), digits))
}

# This function has been commented out.
# Is the computation correct?
income.from.accruals.new = function(eventSeries, by, digits=2, ...) {

  # NominalAccrued has NaN if AD0 is later than IED.
  # This should not be.
  # As workaround, it is set to 0
  # Should be dropped as soon as ACTUS bug is fixed.
  if (is.nan(eventSeries$evs[eventSeries$evs[,"Type"]=="AD0","NominalAccrued"])) {
    eventSeries$evs[eventSeries$evs[,"Type"]=="AD0","NominalAccrued"] <- 0
  }
  ev.df <- as.data.frame(eventSeries)[, c("Date","Type","NominalAccrued")]
  ev.df$Date <- substring(ev.df$Date, 1, 10)
  ev.target <- data.frame(Date = c(as.character(min(by) - 24 * 60 * 60), as.character(by)), Type = c("Init",rep("Accr",length(by))),
                          NominalAccrued = c(0, rep(NA,length(by))))
  colnames(ev.target) <- c("Date", "Type", "NominalAccrued")
  ev.df <- rbind(ev.df, ev.target)
  # ev.ts <- zoo(ev.df, as.Date(ev.df$Date))
  ev.ts <- zoo(ev.df, 1:length(ev.df$Date))
  ev.ts$NominalAccrued <- na.locf(ev.ts$NominalAccrued)
  deltaAccr <- c(diff(as.numeric(coredata(subset(ev.ts, Type == "Accr")$NominalAccrued))))
  return(round(deltaAccr, digits))
}

# income from revaluation
income.from.revaluation = function(object, by, method, digits=2, ...) {
  # compute aggregate principal cash flows which are added/deducted
  # from delta-market-values between by-times
  evs <- events(object, as.character(by[1]))
  dates <- FEMS:::get(evs,"evs")$Date
  types <- FEMS:::get(evs,"evs")$Type
  values <- FEMS:::get(evs,"evs")$Value
  pr.cf <- aggregate(
    timeSeries(
      c(rep(0, length(by)), values[types %in% c("IED","PR","MD")]),
      c(by, timeDate(substring(dates[types %in% c("IED","PR","MD")], 1, 10)))
    ), by = by, FUN = sum)
  
  # compute mark-to-model values
  if (is.null(method)) {
    vals <- FEMS::value(object, as.character(by), type = "market", digits = digits)
  } else {
    vals <- FEMS::value(object, as.character(by), type = "market", 
                         method = method, digits = digits)
  }
  # return first differences
  # inc = as.numeric(c(0, diff(vals)) + pr.cf)
  inc <- as.numeric( diff(vals) + pr.cf[-1])
  return(round(inc, digits))
}

# income from revaluation using eventSeries
income.from.revaluation.from.es = function(evs, by, method, digits=2, ...) {
  # compute aggregate principal cash flows which are added/deducted
  # from delta-market-values between by-times
  dates <- FEMS:::get(evs,"evs")$Date
  types <- FEMS:::get(evs,"evs")$Type
  values <- FEMS:::get(evs,"evs")$Value
  pr.cf <- aggregate(
    timeSeries(
      c(rep(0, length(by)), values[types %in% c("IED","PR","MD")]),
      c(by, timeDate(substring(dates[types %in% c("IED","PR","MD")], 1, 10)))
    ), by = by, FUN = sum)
  
  # compute mark-to-model values
  if (is.null(method)) {
    vals <- FEMS::value(evs, as.character(by), type = "market", digits = digits)
  } else {
    vals <- FEMS::value(evs, as.character(by), type = "market", 
                         method = method, digits = digits)
  }
  
  # return first differences
  # inc = as.numeric(c(0, diff(vals)) + pr.cf)
  inc <- as.numeric( diff(vals) + pr.cf[-1])
  return(round(inc, digits))
}



#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", signature = c("eventList", "timeDate", "missing"),
          definition = function(object, by, type, method, ...){
            return(income(object, by, type="marginal", ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", signature = c("eventList", "timeDate", "character"),
          definition = function(object, by, type, ...){
            return(income(as.data.frame(object), by, type, ...))
          })

#' @include EventSeries.R
#' @export
#' @rdname inc-methods
setMethod(f = "income", signature = c("eventList", "timeDate", "character", 
                                      "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){
            return(income(as.data.frame(object), by, type, revaluation.gains, ...))
          })

#' @export
#' @rdname inc-methods
setMethod(f = "income", signature = c("data.frame", "timeDate", "character"),
          definition = function(object, by, type, method, ...){
            pars=list(...)

            if("select"%in%names(pars)) {
              object=subset(object, ContractID %in% pars[["select"]][[1]])
            }
            
            if (!("revaluation.gains" %in% names(pars) && 
                  pars[["revaluation.gains"]]==FALSE) ) {
              stop("Cannot compute revaluation gains from 'EventSeries' and 'EventTable' objects!")
            } else if ( "tree" %in% names(pars) ) {
              tree = pars[["tree"]]
              leafs = lapply(tree$leafs, FUN=function(x) {
                income( subset( object, ContractID %in% x), by=by, type=type, 
                        revaluation.gains=FALSE)
              })
              inc = FEMS:::aggregate.leafs(leafs, tree$branches, by)
              # if ( is.null(dim(inc)) )
              #   inc = inc[-1]
              # else
              #   inc = inc[,-1]
              
            } else if (type=="marginal" ) {
              ev.raw = subset(as.data.frame(object), 
                              Type %in% c("AD0", "IP", "FP", "OPS", "DPR", "RES"))
              inc = timeSeries(rep(0, length(by)), charvec=by)
              cf.raw = timeSeries(ev.raw$Value,
                                  charvec=substring(ev.raw$Date, 1, 10))
              cf.aggr = aggregate(cf.raw, by, FUN=sum)
              inc[time(cf.aggr),] <- cf.aggr
              inc  = as.numeric(series(inc))
              if ( is.null(dim(inc)) )
                inc = inc[-1]
              else
                inc = inc[,-1]
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal",...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })

## @include
#' @export
#' @rdname inc-methods
setMethod(f = "income", signature = c("data.frame", "timeDate", "character", 
                                      "logical", "missing"),
          definition = function(object, by, type, revaluation.gains, ...){

            pars=list(...)
            if ("digits" %in% names(pars)) {
              digits = pars$digits
            } else {
              digits = 2
            }
            
            if("select" %in% names(pars)) {
              object=subset(object, ContractID %in% pars[["select"]][[1]])
            }
            
            if ( revaluation.gains ) {
              stop("Cannot compute revaluation gains from 'EventSeries' and 'EventTable' objects!")
            } else if ( "tree" %in% names(pars) ) {
              tree=pars[["tree"]]
              leafs = lapply(tree$leafs, FUN=function(x) {
                income( subset( object, ContractID %in% x), by=by, type=type, 
                        revaluation.gains=FALSE, digits=digits)
              })
              inc = FEMS:::aggregate.leafs(leafs, tree$branches, by)
              # if ( is.null(dim(inc)) )
              #   inc = inc[-1]
              # else
              #   inc = inc[,-1]
              
            } else if (type=="marginal" ) {
              ev.raw = subset(as.data.frame(object), 
                              Type %in% c("AD0", "IP", "FP", "OPS", "DPR", "RES"))
              inc = timeSeries(rep(0, length(by)), charvec=by)
              cf.raw = timeSeries(ev.raw$Value,
                                  charvec=substring(ev.raw$Date, 1, 10))
              cf.aggr = aggregate(cf.raw, by, FUN=sum)
              inc[time(cf.aggr),] <- cf.aggr
              inc  = round(as.numeric(series(inc)), digits=digits)
              if ( is.null(dim(inc)) )
                inc = inc[-1]
              else
                inc = inc[,-1]
            } else if(type=="cumulative") {
              inc = cumsum(income(object, by, type="marginal", 
                                  revaluation.gains=FALSE, ...))
            } else {
              stop(paste("Income type '", type, "' not recognized!", sep=""))
            }
            return(inc)
          })



