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
#' @param type A character representing the type of value (either 'nominal' or 'markToModel')
#' 
#' @param method (optional) for type='markToModel' a 'ValuationEngine' can be specified according to which the value is computed
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
#' value(pam,by=ad,type="markToModel")
#' 
#' # again with a different engine
#' set(dcEngine,list(RiskFactorObjectLink="RiskFreeCurve",
#'                   DiscountingSpread=0.1))
#' value(pam,by=ad,type="markToModel",method=dcEngine)
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
            if (type == "nominal") {
              val <- FEMS::value(events(object, by[1]), by , "nominal", ...) 
            } else if (type %in% c("markToModel","markToMarket")) {
              stop("Please specify argument 'method' when computing 'markToModel' for an EventSeries!")
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
            if (type == "nominal") {
              # AD0 has only a single element
              val <- FEMS::value(FEMS::events(object, by), as.character(by), type, ...)
            } else if (type %in% c("markToModel", "markToMarket")) {  
              stop("Please specify argument 'method' when computing 'markToModel' for an EventSeries!")
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
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "nominal", method, ...)
          } else if (type %in% c("markToModel", "markToMarket") ) {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "markToModel", method, ...)
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
            return(FEMS::value(object, as.character(by), type, method, ...))
          })

#' @include ContractType.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "timeBuckets", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            val <- FEMS::value(object, as.character(by), type, method, ...)
            names(val) <- by@breakLabs
            return(val)
          })

#' @include ContractType.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("ContractType", "AD0", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object,ad), as.character(by), type, ...)
            } else if (type %in% c("markToModel","markToMarket")) {
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
            return(FEMS::value(object, as.character(by), type, ...))        
          })



#' @include EventSeries.R
#' @export
#' @rdname val-methods
#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
setMethod(f = "value", signature = c("EventSeries", "timeDate", "character", "missing"),
          definition = function(object, by, type, ...){
            return(FEMS::value(object, as.character(by), type, ...) )
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeBuckets", "character", "missing"),
          definition = function(object, by, type, ...){
            val <- FEMS::value(object, as.character(by), type, ...)
            names(val) <- by@breakLabs
            return(val)
          })


#' @include EventSeries.R
#' @export
#' @rdname val-methods
#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
setMethod(f = "value", signature = c("EventSeries", "timeDate", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            return(FEMS::value(object, as.character(by), type, method, ...) )
          })

#' @include EventSeries.R
#' @include TimeBuckets.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "timeBuckets", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            val <- FEMS::value(object, as.character(by), type, method, ...)
            names(val) <- by@breakLabs
            return(val)
          })


#' @include EventSeries.R
#' @export
#' @rdname val-methods
#' The value is computed from an \code{EventSeries}, 
#' and discounting is carried out by explicitly calling the discouting engine.
setMethod(f = "value", signature = c("EventSeries", "character", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, digits = 2, ...){
            
            ev.df <- as.data.frame(object)
            pars <- list(...)

            if ("filter" %in% names(pars)) {
              ev.df = subset(ev.df, ContractID %in% pars[["filter"]][[1]])
            }
            
            # convert dates to time date
            date <- timeDate(substring(by, 1, 10))
            ev.df$Date <- timeDate(substring(ev.df$Date, 1, 10))
            
            if (type == "nominal") {
              val <- FEMS::value(object, by, "nominal", digits = digits, ...)
              
            } else if ( type %in% c("markToModel", "markToMarket") ) {
              # add spread to interest rate
              spread <- FEMS::get(method, "DiscountingSpread")
              dc <- get(method, "RiskFactorObject")
              FEMS::set(dc, list(Rates = FEMS::get(dc, "Rates") + spread))
              
              ids <- unique(ev.df$ContractID)
              val <- matrix(, nrow = length(ids), ncol = length(date))
              for (t in 1:length(date)) {
                for (i in 1:length(ids)) {
                  evs <- subset(ev.df, ContractID == ids[i] & Date > date[t])
                  if (nrow(evs) == 0) {
                    val[i,t] <- 0.0
                  } else if (sum(evs$Type %in% c("IED", "PRD")) > 0) {
                    val[i,t] <- 0.0
                  } else {
                    cfs <- evs$Payoff
                    dfs <- discountFactors(
                      dc,
                      termStart = substring(as.character(date[t]), 1, 10),
                      termEnd = as.character(evs$Date), isDateEnd = TRUE)
                    val[i,t] <- round(as.numeric(cfs %*% dfs), digits = digits)
                  }
                }
              }

              val <- matrix(val, ncol = length(by))
              colnames(val) <- by
              rownames(val) <- ids
              
              # rebase yield curve
              set(dc, list(Rates = get(dc, "Rates") - spread)) # TODO: implement discounting more consistently
              
              # TODO: Check if this already working for tree structure as well...
              if ("tree" %in% names(pars)) {
                tree = pars[["tree"]]
                leafs = tree$leafs
                val = lapply(leafs, FUN = function(x) {
                  apply(matrix(val[x, ], ncol = length(by)), 2, sum)
                })
                val <- aggregate.leafs(val, tree$branches, type)
                colnames(val) = by
              } else {
                val <- apply(val, 2, sum)
              }
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(round(val, digits))
          })


#' @include EventSeries.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("EventSeries", "character", "character", "missing"),
          definition = function(object, by, type, digits = 2, ...){

              ev.df = as.data.frame(object)
              pars = list(...)
              if ("filter" %in% names(pars)) {
                ev.df = subset(ev.df, ContractID %in% pars[["filter"]][[1]])
              }
              # convert dates to time date
              date <- timeDate(substring(by, 1, 10))
              ev.df$Date <- timeDate(substring(ev.df$Date, 1, 10))
              # compute value
              if (type == "nominal") {
                
                # extract state of nominal value from most-recent event for each contract 
                # and analysis date (i.e. by-vector element)
                ids <- unique(ev.df$ContractID)
                if (is.null(ids)) ids = "noID"
                val <- matrix(, nrow = length(ids), ncol = length(date))
                for (t in 1:length(date)) {
                  for (i in 1:length(ids)) {
                    if (date[t] < min(ev.df$Date)) {
                      val[i,t] <- 0
                    } else {
                      if (ids[i]=="noID")
                        val[i,t] <- tail(subset(ev.df, Date <= date[t]), 1)$NominalValue
                      else
                        val[i,t] <- tail(subset(ev.df, ContractID == ids[i] & Date <= date[t]), 1)$NominalValue
                    }
                  }                
                }
                val <- matrix(val, ncol = length(by))
                colnames(val) <- by
                rownames(val) <- ids
                
                # organize in tree structure (if required)
                if ("tree" %in% names(pars)) {
                  tree <- pars[["tree"]]
                  leafs <- tree$leafs
                  val <- lapply(leafs, FUN = function(x) {
                    apply(matrix(val[x, ], ncol = length(by)), 2, sum)
                  })
                  val <- aggregate.leafs(val, tree$branches, type)
                  colnames(val) <- by
                } else {
                  val <- apply(val, 2, sum)
                }
            } else if (type %in% c("markToModel","markToMarket")) {
              stop("Please specify argument 'method' when computing 'markToModel' for an EventSeries!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep = ""))
            }
            return(round(val, digits))
          })


#################################################################
##### Various methods with object of class Portfolio as Input...

setMethod(f = "value", signature = c("Portfolio", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "nominal", method, ...)
            } else if (type %in% c("markToModel", "markToMarket") ) {
              val <- FEMS::value(FEMS::events(object, by[1]), by, "markToModel", method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "character", "character", "missing"),
          definition = function(object, by, type, method, ...){
            val <- FEMS::value(FEMS::events(object, by[1]), by, type, ...)
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeBuckets", "character", "missing"),
          definition = function(object, by, type, ...){
            val <- FEMS::value(object, as.character(by), type, ...)
            names(val) <- by@breakLabs
            return(val)
          })

setMethod(f = "value", signature = c("Portfolio", "timeBuckets", "character", "ValuationEngine"),
          definition = function(object, by, type, method, ...){
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
            val <- FEMS::value(object, as.character(by), type, method, ...)
            return(val)
          })

