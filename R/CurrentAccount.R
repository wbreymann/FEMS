#' @export
setRefClass("CurrentAccount",
            fields = list(
              ContractID = "character",
              ContractType = "character",
              ContractDealDate = "character",
              Currency = "character",
              CashFlows = "data.frame",
              InternalCashFlows = "data.frame",
              # Instead of a hard-coded mechanism we should provided the possibility 
              # to define an arbitrary function with arbitrary arguments
              # cf. the modification of the CashFlowPattern in class Operations
              # The only specialized mechnism we need is to get the cashflows
              # from a call to liquidity for the whole model, discarding the current account.
              # This may be hard-coded by a specialized function 
              # but we should allow for wome flexibility
              PercentageOutflows = "data.frame", # besser timeSeries?
              CycleAnchorDateOfInterestPayment = "character",
              CycleOfInterestPayment = "character",
              MarketObjectCodeRateReset = "character",
              Compound = "character",
              Period = "character",
              StatusDate = "timeDate",
              NotionalPrincipal = "numeric",
              AccruedInterest = "numeric",
              rf_connector = "RiskFactorConnector",
              val_engine = "ValuationEngine"
            ))
# Grundsätzliche Bemerkung:
# Du benutzt für Zeitreihen einen "data.frame". Das ist ungünstig.
# Es gibt eine Reihe von Zeitreihenklassen in R.
# Nils hat "zoo" und "timeSeries" benutzt.
# Ich schlage vor, dass wir als Zeitklasse "timeDate" und als Zeitreihenklassen
# "timeSeries" benutzen. "zoo" kenne ich nicht wirklich.

#' @export
setGeneric(name = "CurrentAccount",
           def = function(...){
             standardGeneric("CurrentAccount")
           })

#' @export
setMethod(f = "CurrentAccount",signature = c(),
          definition = function(...){
            object <- new("CurrentAccount")
            pars <- list(...,
                         ContractType="CurrentAccount",
                         CashFlows=data.frame(),
                         InternalCashFlows=data.frame(),
                         PercentageOutflows=data.frame(),
                         Compound="compound",
                         Period="Y",
                         StatusDate = timeDate("0000-01-01"),
                         NotionalPrincipal = 0,
                         AccruedInterest = 0)
            if(length(pars)==0){
            }  else if (is.list(pars[[1]])) {
              FEMS:::set(object, pars[[1]])
            } else {
              FEMS:::set(object, pars)
            }
            return(object)
          })

#' @export
setMethod(f = "set", signature = c("CurrentAccount","list"),
          function(object, what, ...){
            silent <- lapply(names(what),function(x) object$field(x,what[[x]]))
          })

#' @include ValuationEngine.R
#' @export
#' @rdname set-methods
setMethod(f = "set", signature = c("ContractType","ValuationEngine"),
          definition = function(object, what){
            object$val_engine <- what
          })

#' @include RiskFactorConnector.R
#' @export
setMethod(f = "set", signature = c("CurrentAccount", "RiskFactorConnector"),
          definition = function(object, what){
            object$rf_connector <- what
          })

#' @export
setMethod(f = "get", signature = "CurrentAccount",
          function(object, what, ...){
            # currently not working to return data.frames
            fields <- sapply(what,function(x) object$field(x))
            return(fields)
          })

#' @export
setGeneric(name = "add.cashflow",
           def = function(object, added_cf, type){
             standardGeneric("add.cashflow")
           })

#' @export
setMethod(f = "add.cashflow", signature = c("CurrentAccount", "data.frame"),
          definition = function(object, added_cf, type = "external"){
            # extend the cash-flows in existing object
            if (type == "internal") {
              cf_prev <- object$InternalCashFlows
            } else {
              cf_prev <- object$CashFlows
            }
            cf_prev$temp_name <- rownames(cf_prev)
            added_cf$temp_name <- rownames(added_cf)
            
            # if previous df is not empty, bind the two dataframes together by row and aggregate
            if ((dim(cf_prev)[1]==0)) {
              agg <- added_cf
            } else {
              agg <- aggregate(. ~ temp_name, rbind(cf_prev,setNames(added_cf, names(cf_prev))), sum)
            }
            
            # reformat again 
            rownames(agg) <- agg$temp_name
            cf_new <- agg[!(names(agg) %in% "temp_name")]
            if (type == "internal") {
              object$InternalCashFlows <- cf_new
            } else {
              object$CashFlows <- cf_new
            }
          })

#' @export
setGeneric(name = "add.internalcashflow",
           def = function(object, added_cf){
             standardGeneric("add.internalcashflow")
           })

#' @export
setMethod(f = "add.internalcashflow", signature = c("CurrentAccount", "data.frame"),
          definition = function(object, added_cf){
            add.cashflow(object, added_cf, type = "internal")
          })

#' @include Events.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "RiskFactorConnector"),
          definition = function(object, ad, model, end_date){
            if (missing(end_date)) {
              stop("ErrorIn::CurrentAccount::events:: end_date needs to be provided !!! ")
            }
            return(FEMS:::EventSeries(object, ad, model, end_date=end_date))
          })

#' @export
setMethod(f = "EventSeries", signature = c("CurrentAccount", "character"),
          definition = function(object, ad, model, end_date, ...){
            
            # create event series object
            out <- new("EventSeries")
            out$id <- FEMS:::get(object,"ContractID")
            out$ct <- FEMS:::get(object,"ContractType")
            
            # evaluate reserving pattern
            out$evs <- currentaccount.evs(object, model, end_date, object$Compound, object$Period)
            out$evs <- out$evs[out$evs$Date>=ad,] # inefficient, should be done in function call...
            out$evs <- out$evs[out$evs$Date<=end_date,]
            ## Attach EventSeries to contract

            return(out)
          })

currentaccount.evs <- function(object, model, end_date, method, period){
  
  # get the relevant yield curve from the risk factor connector
  yc <- get(model, object$MarketObjectCodeRateReset)
  
  # get dates for interest payments
  interest_dates <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, 
                                         object$CycleOfInterestPayment, end_date)
  next_rate_dt <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, 
                                       object$CycleOfInterestPayment,
                                       as.character(as.Date(end_date) %m+% years(10)))
  next_rate_dt <- min(next_rate_dt[next_rate_dt>max(interest_dates)])
  
  # get a combination of all dates
  all_dates <- sort(unique(c(object$ContractDealDate,
                             interest_dates,
                             rownames(object$CashFlows),
                             rownames(object$InternalCashFlows),
                             rownames(object$PercentageOutflows),
                             end_date)))
  if (min(c(all_dates,object$ContractDealDate)) < yc$ReferenceDate[1]) {
    stop("ErrorIn::CurrentAccount:: Dates must all lay after first ReferenceDate of YieldCurve !!! ")
  }

  # to use a shorter name
  ccy <- object$Currency
  
  # preparing loop
  time <- 0
  nominal_value <- 0
  nominal_accrued <- 0
  nominal_rate <- 0
  ev_tbl <- data.frame()
  rate_count <- 1
  for (i in 1:length(all_dates)) {
    next_ev <- data.frame()
    if (i==1){
      if (all_dates[i] %in% interest_dates) {
        tryCatch({
          nominal_rate <- rates2(yc, interest_dates[rate_count+1], interest_dates[rate_count], isDateEnd=TRUE)
        }, error = function(e) {
          nominal_rate <- rates2(yc, next_rate_dt, interest_dates[rate_count], isDateEnd=TRUE)
        })
        rate_count <- rate_count + 1
      }
      if (all_dates[i] %in% object$ContractDealDate){
        nominal_value <- object$NotionalPrincipal
        nominal_accrued <- object$AccruedInterest
        next_ev <- rbind(next_ev,
                        data.frame(Date=object$ContractDealDate, Value=nominal_value, Type="AD0", Level="P", Currency=ccy,
                                   Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                   NominalAccrued=nominal_accrued))
      }
      if (all_dates[i] %in% rownames(object$CashFlows)){
        value <- object$CashFlows[all_dates[i],]
        nominal_value <- value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="AM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$InternalCashFlows)){
        value <- object$InternalCashFlows[all_dates[i],]
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="IAM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$PercentageOutflows)) {
        value <- -nominal_value * object$PercentageOutflows[all_dates[i],]
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="AM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }
    } else {
      if (all_dates[i] %in% interest_dates) {
        tryCatch({
          nominal_rate <- rates2(yc, interest_dates[rate_count+1], interest_dates[rate_count], isDateEnd=TRUE)
        }, error = function(e) {
          nominal_rate <- rates2(yc, next_rate_dt, interest_dates[rate_count], isDateEnd=TRUE)
        })
        rate_count <- rate_count + 1
      }
      time <- as.numeric((as.timeDate(all_dates[i])-as.timeDate(all_dates[1]))/365)
      df_s <- discountFactorsv2(yc, all_dates[i-1], all_dates[i], method=method, period=period)
      nominal_accrued <- nominal_accrued + (df_s-1) * nominal_value
      
      if (all_dates[i] %in% interest_dates) {
        value <- nominal_accrued
        nominal_value <- nominal_value + nominal_accrued
        nominal_accrued <- 0
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="IPIC", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }  
      if (all_dates[i] %in% rownames(object$CashFlows)) {
        value <- object$CashFlows[all_dates[i],]
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="AM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$InternalCashFlows)) {
        value <- object$InternalCashFlows[all_dates[i],]
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="IAM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$PercentageOutflows)) {
        value <- -object$PercentageOutflows[all_dates[i],] * nominal_value
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], Value=value, Type="AM", Level="P", Currency=ccy,
                                    Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }
    }
    if (i == length(all_dates) & length(next_ev)==0) {
      next_ev <- data.frame(Date=all_dates[i], Value=0, Type="AM", Level="P", Currency=ccy,
                            Time=time, NominalValue=nominal_value, NominalRate=nominal_rate,
                            NominalAccrued=nominal_accrued)
    }
    ev_tbl <- rbind(ev_tbl, next_ev)
    }
    return(ev_tbl)
}


################## Analysis functions ###################################################
# Liquidity methods...

#' @include Liquidity.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("CurrentAccount", "timeDate", "missing"),
          definition = function(object, by, type, digits = 2) {
            return(liquidity(object, by, type = "marginal", digits = digits))
          })

#' @include TimeBuckets.R Liquidity.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("CurrentAccount", "timeBuckets", "missing"),
          definition = function(object, by, type, digits = 2) {
            liq <- liquidity(object, as.timeDate(by), type = "marginal", digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })

#' @include TimeBuckets.R Liquidity.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("CurrentAccount", "timeBuckets", "character"),
          definition = function(object, by, type, digits=2) {
            liq <- liquidity(object, as.timeDate(by), type = type, digits = digits)
            names(liq) <- by@bucketLabs
            return(liq)
          })

#' @include Liquidity.R
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("CurrentAccount", "timeDate", "character"),
          definition = function(object, by, type, digits = 2) {
            evs <- events(object, as.character(by[1]), 
                          object$rf_connector, end_date=as.character(by[length(by)]))
            evs$evs <- evs$evs[!(evs$evs$Type == "IAM"),]
            liq <- liquidity(evs, by, type, digits=digits)
            return(liq)
          })

########################################################################################
# Value methods...

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "character", "character", "missing"),
          definition = function(object, by, type, method, end_date, ...){
            val <- value(object, by, type, object$rf_connector, end_date=end_date)
            return(val)
          })

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, end_date, ...){
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1], object$rf_connector, end_date=end_date), by, "nominal", method, ...)
            } else if (type %in% c("markToModel", "markToMarket") ) {
              val <- FEMS::value(FEMS::events(object, by[1], object$rf_connector, end_date=end_date), by, "markToModel", method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })


get.dates.from.cycle <- function(anchor_date, cycle, end_date){
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  tSeq <- timeSequence(anchor_date, end_date, by = by)
  return(as.character(tSeq))
}







