#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' Derive the sensitivity of a contract's value with respect
#' to a change in risk factors
#'
#' Different sensitivity-concepts can be derived from a financial
#' instrument or its analytical value, respectively. Currentently, 
#' these are Macaulay and Fisher-Weil for Maturity-contracts and
#' the Numerical sensitivity for all kinds of contracts.
#'
#' Macaulay Duration is a measure for the sensitivity of the net present
#' value of a Maturity instrument to a parallel Shift in the related
#' yield curve. This measure is based on the discounted cash flows of the 
#' instrument.
#' 
#' Fisher-Weil Duration is a measure for the sensitivity of the net present
#' value of a Maturity instrument to a parallel Shift in the related
#' yield curve. This measure is based on the discounted cash flows of the 
#' instrument.
#' 
#' Numerical Sensitivity is the percentage change in the contract's value
#' (according to a specific valuation method or 'ValuationEngine', 
#' respectively) under an arbitrary alteration of the market environment
#' as represented by the 'RiskFactorConnector'.
#'
#' @param object object The \code{ContractType} or \code{EventSeries}-object for 
#'   which to compute sensitivity
#' 
#' @param type A character representing the type of sensitivity 
#'   (either 'macaulay', 'fisherweil', or 'numeric')
#' 
#' @param method (optional) a 'ValuationEngine' can be specified according to 
#'   which the value is computed
#' 
#' @param scenarios For type='numeric' a list of 'RiskFactorConnector'-objects 
#'   where the first element represents the base scenario and all other elements alterations thereof 
#'
#' @param ... Currently unused
#'
#' @return A \code{numeric} object giving the sensitivity
#' 
#' @seealso \code{\link{ContractType}} and \code{\link{ValuationEngine}} 
#' and \code{\link{value}} and \code{\link{RiskFactorConnector}} 
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
#' ad <- "2016-06-02T00"
#' yc <- YieldCurve()
#' set(yc, what=list(Nodes=list(ReferenceDate="2016-06-01T00", 
#'                              Tenors=c("1D", "10Y"), 
#'                              Rates=c(0.005, 0.02)), 
#'                   MarketObjectCode = "RiskFreeCurve"))
#' rf <- RFConn()
#' add(rf, yc)
#' dcEngine <- DcEngine()
#' set(dcEngine, list(RiskFactorObjectLink="RiskFreeCurve", 
#'                   DiscountingSpread=0.0))
#' set(dcEngine, rf)
#' 
#' # compute events
#' evs=events(pam,ad)
#' 
#' # compute sensitivity with Macaulay duration
#' sensitivity(pam, by=ad, type="macaulay", method=dcEngine)
#' sensitivity(evs, by=ad, type="macaulay", method=dcEngine)
#' 
#' # compute sensitivity with Fisher-Weil duration
#' sensitivity(pam, by=ad, type="fisherweil", method=dcEngine)
#' sensitivity(evs, by=ad, type="fisherweil", method=dcEngine)
#' 
#' # Default is with Fisher-Weil duration
#' sensitivity(pam, by=ad, method=dcEngine)
#' sensitivity(evs, by=ad, method=dcEngine)
#' 
#' # compute numeric sensitivity
#' # only possible from object of class EventSeries
#' # define base-scenario
#' rf.base = rf
#' # define shift-scenario
#' yc.shift <- YieldCurve()
#' set(yc.shift, what=list(Nodes=list(ReferenceDate="2016-06-01T00", 
#'                              Tenors=c("1D", "10Y"), 
#'                              Rates=c(0.005, 0.02)+0.1), 
#'                   MarketObjectCode = "RiskFreeCurve"))
#' rf.shift <- RFConn()
#' add(rf.shift, yc.shift)
#' # compute sensitivity
#' sensitivity(evs, by=ad, type="numeric", method=dcEngine, scenarios=c(rf.base, rf.shift))
#' 
## @include
#' @export
#' @docType methods
#' @rdname sen-methods
setGeneric(name = "sensitivity", 
           def = function(object, by, type, method, scenarios, ...) {
  standardGeneric("sensitivity")
})

#' #' @include ContractType.R
#' #' @export
#' #' @rdname sen-methods
#' setMethod(f="sensitivity", 
#'           signature = c("ContractType", "character", "missing", "missing", "missing"), 
#'           definition = function(object, by, type, method, scenarios) {
#'             # set default type
#'             type = "fisherweil"
#'             
#'             if (type=="macaulay") {
#'               
#'               D = sensitivity(EventSeries(object, by[1]), by, type, 
#'                               DcEngine(get(object, "ValuationEngine")))
#'               
#'             } else if (type=="fisherweil") {
#'               
#'               D = sensitivity(EventSeries(object, by[1]), by, type, 
#'                               DcEngine(get(object, "ValuationEngine")))
#'               
#'             } else if (type=="numeric") {
#'               
#'               stop("Needs a vector of scenarios in order to compute numeric sensitivity!")
#'               
#'             } else {
#'               stop(paste("Value type '", type, "' not recognized!", sep=""))
#'             }
#'               return(D)
#'           })

#' @include ContractType.R
#' @export
#' @rdname sen-methods
setMethod(f="sensitivity", 
          signature = c("ContractType", "character", "character", "missing", "missing"), 
          definition = function(object, by, type, method, scenarios) {
            
            if (type=="macaulay") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, 
                              DcEngine(get(object, "ValuationEngine")))
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, 
                              DcEngine(get(object, "ValuationEngine")))
              
            } else if (type=="numeric") {
              
              stop("Needs a vector of scenarios in order to compute numeric sensitivity!")
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include ContractType.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname sen-methods
setMethod(f="sensitivity", 
          signature=c("ContractType", "character", "character", "missing", "list"), 
          definition=function(object, by, type, method, scenarios) {
            
            if (type=="macaulay") {
              
              D=sensitivity(EventSeries(object, by[1]), by, type, 
                            DcEngine(get(object, "ValuationEngine")))
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, 
                              DcEngine(get(object, "ValuationEngine")))
              
            } else if (type=="numeric") {
              
              D=sensitivity(EventSeries(object, by[1]), by, type, 
                            DcEngine(get(object, "ValuationEngine")), scenarios)
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include ContractType.R
#' @include ValuationEngine.R
#' @export
#' @rdname sen-methods
setMethod(f="sensitivity", 
          signature=c("ContractType", "character", "missing", "ValuationEngine", "missing"), 
          definition=function(object, by, type, method, scenarios) {
            # set default type
            type = "fisherweil"
            
            if (type=="macaulay") {
              
              D = sensitivity(EventSeries(object, by[1]) , by, type, method)
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, method)
              
            } else if (type=="numeric") {
              
              stop("Need a vector of scenarios in order to compute numeric sensitivity!")
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include ContractType.R
#' @include ValuationEngine.R
#' @export
#' @rdname sen-methods
setMethod(f="sensitivity", 
          signature=c("ContractType", "character", "character", "ValuationEngine", "missing"), 
          definition=function(object, by, type, method, scenarios) {
            
            if (type=="macaulay") {
              
              D=sensitivity(EventSeries(object, by[1]) , by, type, method)
              
            } else if (type=="fisherweil") {
              
              D=sensitivity(EventSeries(object, by[1]), by, type, method)
              
            } else if (type=="numeric") {
              
              stop("Need a vector of scenarios in order to compute numeric sensitivity!")
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include ContractType.R
#' @include ValuationEngine.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname sen-methods
setMethod(f = "sensitivity", 
          signature = c("ContractType", "character", "character", "ValuationEngine", "list"), 
          definition = function(object, by, type, method, scenarios) {
            
            if (type=="macaulay") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, method)
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(EventSeries(object, by[1]), by, type, method)
              
            } else if (type=="numeric") {
              
              # Calculate value for base scenario
              set(method, scenarios[[1]])
              V.0 = FEMS::value(object, by[1], type="markToModel", method=method)
              
              # Calculating the discounted value for all other scenarios
              scenarios[1] = NULL
              V = unlist(lapply(scenarios, FUN=function(x) {
                temp = set(method, x)
                FEMS::value(object, by[1], type="markToModel", method=temp)
              }))
              
              # compute sensitivity
              D = (V - V.0)/V.0
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include EventSeries.R
#' @export
#' @rdname sen-methods
setMethod(
  f = "sensitivity", 
  signature = c("EventSeries",  "character", "character",  "missing",  "missing"), 
  definition = function(object, by, type, method, scenarios) {
            
    if (type=="macaulay") {
              
      stop("Need a 'ValuationEngine' in order to compute Macaulay-sensitivity for an 'EventSeries!")
              
    } else if (type=="fisherweil") {
              
      stop("Need a 'ValuationEngine' in order to compute Fisher-Weil-sensitivity for an 'EventSeries!")
              
    } else if (type=="numeric") {
              
      stop("Need a vector of scenarios in order to compute numeric sensitivity!")
              
    } else {
      stop(paste("Value type '", type, "' not recognized!", sep=""))
    }
    return(D)
  })

#' @include ContractType.R
#' @include ValuationEngine.R
#' @export
#' @rdname sen-methods
setMethod(f="sensitivity", 
          signature=c("EventSeries", "character", "missing", "ValuationEngine", "missing"), 
          definition=function(object, by, type, method, scenarios) {
            # set default type
            type = "fisherweil"
            
            if (type=="macaulay") {
              
              D = sensitivity(object, by, type, method)
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(object, by, type, method)
              
            } else if (type=="numeric") {
              
              stop("Need a vector of scenarios in order to compute numeric sensitivity!")
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })

#' @include EventSeries.R
#' @include ValuationEngine.R
#' @export
#' @rdname sen-methods
setMethod(
  f = "sensitivity", 
  signature = c("EventSeries", "character", "character", "ValuationEngine", "missing"), 
  definition = function(object, by, type, method, scenarios) {
    
    if (type=="macaulay") {
              
      ## cash flow data
      dates <- get(object, "evs")$Date
      t <- get(object, "evs")$Time
      cfs <- get(object, "evs")$Value
              
      ## discount factors
      ad0 <- by[1]
      yc <- YieldCurve(get(method, "InterestRatesModel"))
      dfs <- discountFactors(yc, dates, ad0, isDateEnd = TRUE)
              
      ## Calculating the Macauly Duration
      D <- as.numeric(( t * cfs ) %*% dfs * ( 1 / cfs %*% dfs ))
              
    } else if (type=="fisherweil") {
              
      ## cash flow data
      dates <- get(object, "evs")$Date
      t <- get(object, "evs")$Time
      cfs <- get(object, "evs")$Value
              
      ## discount factors
      ad0 <- by[1]
      yc <- YieldCurve(get(method, "InterestRatesModel"))
      dfs <- discountFactors(yc, dates, ad0, isDateEnd=TRUE)
      s <- rates(yc, dates, isDateEnd=TRUE)
              
      ## Calculating the Fisher-Weil Duration
      D <-  as.numeric(( ( t * cfs ) %*% exp( -s*t ) ) * ( 1 / cfs %*% dfs ))
    } else if (type=="numeric") {
              
      stop("Need a vector of scenarios in order to compute numeric sensitivity!")
              
    } else {
      stop(paste("Value type '", type, "' not recognized!", sep=""))
    }
    
    # We return the sensitivity 
    # An optional parameter should be introduced so that the user can choose
    D = -D        
    return(D)
  })

#' @include EventSeries.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname sen-methods
setMethod(
  f = "sensitivity", 
  signature = c("EventSeries", "character", "character", "missing", "list"), 
  definition = function(object, by, type, method, scenarios) {
            
    if (type=="macaulay") {
              
      stop("Need a 'ValuationEngine' in order to compute Macaulay-sensitivity for an 'EventSeries!")
        
    } else if (type=="fisherweil") {
              
      stop("Need a 'ValuationEngine' in order to compute Fisher-Weil-sensitivity for an 'EventSeries!")
              
    } else if (type=="numeric") {
              
      stop("Need a 'ValuationEngine' in order to compute numeric-sensitivity for an 'EventSeries!")
              
    } else {
      stop(paste("Value type '", type, "' not recognized!", sep=""))
    }
            
  })

#' @include EventSeries.R
#' @include ValuationEngine.R
#' @include RiskFactorConnector.R
#' @export
#' @rdname sen-methods
setMethod(f = "sensitivity", 
          signature = c("EventSeries", "character", "character", "ValuationEngine", "list"), 
          definition = function(object, by, type, method, scenarios) {
            
            if (type=="macaulay") {
              
              D = sensitivity(object, by, type, method)
              
            } else if (type=="fisherweil") {
              
              D = sensitivity(object, by, type, method)
              
            } else if (type=="numeric") {
              
              # Calculate value for base scenario
              dates <- get(object, "evs")$Date
              cfs <- get(object, "evs")$Value
              ad0 <- by[1]
              yc.nme = get(method, "RiskFactorObjectLink")
              yc <- FEMS:::get(scenarios[[1]], yc.nme)
              dfs <- discountFactors(yc, dates, ad0, isDateEnd=TRUE)
              V.0 = as.numeric(cfs%*%dfs)
              
              # Calculating the discounted value for all other scenarios
              scenarios[1] = NULL
              V = unlist(lapply(scenarios, FUN=function(x) {
                yc = FEMS:::get(x, yc.nme)
                dfs <- discountFactors(yc, dates, ad0, isDateEnd=TRUE)
                cfs%*%dfs
              }))
              
              # compute sensitivity
              D = (V-V.0)/V.0
              
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(D)
          })