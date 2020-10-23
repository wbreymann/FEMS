#' @include FEMSContract.R
#' @export
setRefClass("CurrentAccount",
            contains = "FEMSContract",
            fields = list(
              ContractID = "character",
              ContractType = "character",
              ContractDealDate = "character",
              Currency = "character",
              ExternalTransactions = "timeSeries",
              InternalTransfers = "timeSeries",
              PercentageOutflows = "timeSeries",
              CycleAnchorDateOfInterestPayment = "character",
              CycleOfInterestPayment = "character",
              CycleOfRateReset = "character",
              CycleAnchorDateOfRateReset = "character",
              NominalInterestRate = "numeric",
              MarketObjectCodeRateReset = "character",
              Compound = "character",
              Period = "character",
              StatusDate = "character",
              Balance = "numeric",
              AccruedInterest = "numeric",
              RiskFactorConnector = "RiskFactorConnector",
              val_engine = "ValuationEngine"
            ))

#' @export
setGeneric(name = "CurrentAccount",
           def = function(...){
             standardGeneric("CurrentAccount")
           })

#' @export
setMethod(f = "CurrentAccount", signature = c(),
          definition = function(...){
            object <- new("CurrentAccount")
            pars <- list(...,
                         ContractType = "CurrentAccount",
                         Compound ="compound",
                         Period = "Y",
                         StatusDate = "0000-01-01",
                         Balance = 0,
                         AccruedInterest = 0,
                         NominalInterestRate = 0,
                         Currency = "CHF")
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
            silent <- lapply(names(what), function(x) object$field(x, what[[x]]))
          })

#' @export
setMethod(f = "set", signature = c("CurrentAccount","missing"),
          function(object, what, ...){
            pars <- list(...)
            silent <- lapply(names(pars), function(x) object$field(x, pars[[x]]))
          })

#' @include RiskFactorConnector.R
#' @export
setMethod(f = "set", signature = c("CurrentAccount", "RiskFactorConnector"),
          definition = function(object, what){
            object$RiskFactorConnector <- what
          })

#' @export
setGeneric(name = "add.cashflow",
           def = function(object, added_cf, ...){
             standardGeneric("add.cashflow")
           })

#' @export
setMethod(f = "add.cashflow", signature = c("CurrentAccount", "timeSeries"),
          definition = function(object, added_cf, type = "external"){
            # extend the cash-flows in existing object
            if (type == "internal") {
              cf_prev <- object$InternalTransfers
            } else if (type == "external") {
              cf_prev <- object$ExternalTransactions
            } else {
              stop("ErrorIn::CurrentAccount:: Type of cashflow not allowed !!!")
            }
            
            # if previous df is not empty, bind the two dataframes together by row and aggregate
            if ((dim(cf_prev)[1]==0)) {
              agg <- added_cf
            } else {
              comb <- rbind(cf_prev, added_cf)
              agg <- aggregate(comb, time(comb), "sum")
            }
            
            # reformat again 
            if (type == "internal") {
              object$InternalTransfers <- agg
            } else {
              object$ExternalTransactions <- agg
            }
          })

#' @export
setGeneric(name = "add.externaltransaction",
           def = function(object, added_cf){
             standardGeneric("add.externaltransaction")
           })

#' @export
setMethod(f = "add.externaltransaction", signature = c("CurrentAccount", "timeSeries"),
          definition = function(object, added_cf){
            add.cashflow(object, added_cf, type = "external")
          })

#' @export
setGeneric(name = "add.internaltransfer",
           def = function(object, added_cf){
             standardGeneric("add.internaltransfer")
           })

#' @import timeSeries
#' @export
setMethod(f = "add.internaltransfer", signature = c("CurrentAccount", "timeSeries"),
          definition = function(object, added_cf){
            add.cashflow(object, added_cf, type = "internal")
          })

#' @export
setMethod(f = "add.internaltransfer", signature = c("CurrentAccount", "data.frame"),
          definition = function(object, added_cf){
            add.cashflow(object, timeSeries::as.timeSeries(added_cf), type = "internal")
          })

#' @export
setMethod(f = "show", signature = c("CurrentAccount"),
          definition = function(object){
            print("CurrentAccount:")
            cat(paste0("DealDate: ", object$ContractDealDate,"\n"))
            cat(paste0("Currency: ", object$Currency,"\n"))
            cat(paste0("Balance: ", object$Balance,"\n"))
            cat(paste0("Accrued: ", object$AccruedInterest,"\n"))
            cat(paste0("InterestRate: ", round(object$NominalInterestRate*100, 2),"%","\n"))
            if (!dim(object$ExternalTransactions)[1] == 0){
              print("ExternalTransactions:")
              print(object$ExternalTransactions)
            }
            if (!dim(object$InternalTransfers)[1] == 0){
              print("InternalTransfers:")
              print(object$InternalTransfers)
            }
            if (!dim(object$PercentageOutflows)[1] == 0){
              print("Outflows:")
              print(object$PercentageOutflows)
            }
          })

#' @include Events.R 
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "missing"),
          definition = function(object, ad, model, end_date){
            if (missing(end_date)) {
              stop("ErrorIn::CurrentAccount::events:: end_date needs to be provided !!! ")
            }
            return(FEMS:::EventSeries(object, ad, RFConn(), end_date = end_date))
          })

#' @include Events.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "RiskFactorConnector"),
          definition = function(object, ad, model, end_date){
            # currently calculates the entire event series
            # could be made more efficient using StatusDate

            if (missing(end_date)) {
              stop("ErrorIn::CurrentAccount::events:: end_date needs to be provided !!! ")
            }
            return(FEMS:::EventSeries(object, ad, model, end_date=end_date))
          })

#' @include Events.R YieldCurve.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "YieldCurve"),
          definition = function(object, ad, model, end_date){
            # currently calculates the entire event series
            # could be made more efficient using StatusDate

            rf1 <- RFConn(model)
            if (missing(end_date)) {
              stop("ErrorIn::CurrentAccount::events:: end_date needs to be provided !!! ")
            }
            return(FEMS:::EventSeries(object, ad, rf1, end_date=end_date))
          })

#' @include Events.R DynamicYieldCurve.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "DynamicYieldCurve"),
          definition = function(object, ad, model, end_date){
            # currently calculates the entire event series
            # could be made more efficient using StatusDate

            rf1 <- RFConn(model)
            if (missing(end_date)) {
              stop("ErrorIn::CurrentAccount::events:: end_date needs to be provided !!! ")
            }
            return(FEMS:::EventSeries(object, ad, rf1, end_date=end_date))
          })

#' @export
setMethod(f = "EventSeries", signature = c("CurrentAccount", "character"),
          definition = function(object, ad, model, end_date, ...){

            if (class(model)=="YieldCurve"){
              model <- RFConn(model)
            }
            
            # create event series object
            out <- new("EventSeries")
            out$id <- as.character(FEMS:::get(object,"ContractID"))
            out$ct <- as.character(FEMS:::get(object,"ContractType"))
            
            # evaluate reserving pattern
            out$evs <- currentaccount.evs(object, model, end_date, object$Compound, object$Period)
            out$evs <- out$evs[out$evs$Date>=ad,] # inefficient, should be done in function call...
            out$evs <- out$evs[out$evs$Date<=end_date,]
            ## Attach EventSeries to contract

            return(out)
          })

currentaccount.evs <- function(object, model, end_date, method, period){

  # get the relevant yield curve from the risk factor connector
  if (length(object$MarketObjectCodeRateReset)==0){
    yc <- object$NominalInterestRate
  } else {
      yc <- get(model, object$MarketObjectCodeRateReset)
  }
  
  # get dates for interest payments
  if (end_date < object$CycleAnchorDateOfInterestPayment) {
    interest_dates <- object$CycleAnchorDateOfInterestPayment
  } else {
    interest_dates <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, 
                                           object$CycleOfInterestPayment, end_date)
  }
  next_rate_dt <- as.character(timeSequence(interest_dates[length(interest_dates)], 
                               by = convert.cycle(object$CycleOfInterestPayment), 
                               length.out=2)[2])
  
  # get a combination of all dates
  all_dates <- sort(unique(c(object$ContractDealDate,
                             interest_dates,
                             rownames(object$ExternalTransactions),
                             rownames(object$InternalTransfers),
                             rownames(object$PercentageOutflows),
                             end_date)))
  # if (min(c(all_dates, object$ContractDealDate)) < yc$ReferenceDate[1]) {
  #   stop("ErrorIn::CurrentAccount:: Dates must all lay after first ReferenceDate of YieldCurve !!! ")
  # }

  # get the relevant rates as of the rate reset schedule dates...
  if (!identical(object$CycleAnchorDateOfRateReset, character(0)) &
      !identical(object$CycleOfRateReset, character(0))) {
    r <- get.data.rate.reset(yc, object$CycleAnchorDateOfRateReset, 
                                     object$CycleOfRateReset, 
                                     end_date, ISO = FALSE)
    r <- data.frame(Dates = rownames(r),
                    Values = r$Values)
    deal_date_r <- data.frame(Dates = object$ContractDealDate,
                              Values = object$NominalInterestRate)
    if (!(deal_date_r$Dates %in% r$Dates)) {
      r <- rbind(deal_date_r, r)
    }
    # WHY DID WE DO THIS?
    # } else {
    #   r[r$Dates==deal_date_r$Dates, ]$Values  <- deal_date_r$Values
    # }
    
    #potentially drop the ones not being unique
  } else {
    # still data from yc and NominalInterestRate could not match
    r <- data.frame(Dates = object$ContractDealDate,
                    Values = object$NominalInterestRate)
  }
  
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
      nominal_rate <- r[max(which(r$Dates<=interest_dates[rate_count])), ]$Values
      if (all_dates[i] %in% interest_dates) {
        rate_count <- rate_count + 1
      }
      if (all_dates[i] %in% object$ContractDealDate){
        nominal_value <- object$Balance
        nominal_accrued <- object$AccruedInterest
        next_ev <- rbind(next_ev,
                        data.frame(Date=object$ContractDealDate, 
                                   Value=nominal_value, 
                                   Type="AD0", 
                                   Level="P", 
                                   Currency=object$Currency,
                                   Time=time, 
                                   NominalValue=nominal_value, 
                                   NominalRate=nominal_rate,
                                   NominalAccrued=nominal_accrued))
      }
      if (all_dates[i] %in% rownames(object$ExternalTransactions)){
        value <- as.numeric(object$ExternalTransactions[all_dates[i],])
        nominal_value <- value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ETA", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$InternalTransfers)){
        value <- as.numeric(object$InternalTransfers[all_dates[i],])
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ITF", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$PercentageOutflows)) {
        value <- -nominal_value * as.numeric(object$PercentageOutflows[all_dates[i],])
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ETA", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }
    } else {
      r_last <- nominal_rate
      df_s <- discountFactors(r_last, to=all_dates[i-1], from=all_dates[i], method = method, period = period)
      if (all_dates[i] %in% interest_dates) {
        nominal_rate <- r[max(which(r$Dates<=interest_dates[rate_count])), ]$Values
        rate_count <- rate_count + 1
      }
      time <- yearFraction(all_dates[1], all_dates[i], convention = "30E360")
      nominal_accrued <- nominal_accrued * df_s + (df_s-1) * nominal_value
      
      if (all_dates[i] %in% interest_dates) {
        value <- nominal_accrued
        nominal_value <- nominal_value + nominal_accrued
        nominal_accrued <- 0
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="IPCI", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }  
      if (all_dates[i] %in% rownames(object$ExternalTransactions)) {
        value <- as.numeric(object$ExternalTransactions[all_dates[i],])
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ETA", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$InternalTransfers)) {
        value <- 0
        nominal_value <- nominal_value + as.numeric(object$InternalTransfers[all_dates[i],])
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ITF", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      } 
      if (all_dates[i] %in% rownames(object$PercentageOutflows)) {
        value <- -as.numeric(object$PercentageOutflows[all_dates[i],]) * nominal_value
        nominal_value <- nominal_value + value
        next_ev <- rbind(next_ev,
                         data.frame(Date=all_dates[i], 
                                    Value=value, 
                                    Type="ETA", 
                                    Level="P", 
                                    Currency=object$Currency,
                                    Time=time, 
                                    NominalValue=nominal_value, 
                                    NominalRate=nominal_rate,
                                    NominalAccrued=nominal_accrued))
      }
    }
    if (i == length(all_dates) & length(next_ev)==0) {
      next_ev <- data.frame(Date=all_dates[i], 
                            Value=0, 
                            Type="ETA", 
                            Level="P", 
                            Currency=object$Currency,
                            Time=time, 
                            NominalValue=nominal_value, 
                            NominalRate=nominal_rate,
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
            filtered <- c("DPR", "ITF","RES","IPCI")
            evs <- events(object, as.character(by[1]), 
                          object$RiskFactorConnector, end_date=as.character(by[length(by)]))
            evs$evs <- evs$evs[!is.element(evs$evs$Type, filtered),]
            liq <- liquidity(evs, by, type, digits=digits)
            return(liq)
          })

########################################################################################
# Value methods...

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "timeDate", "character", "missing"),
          definition = function(object, by, type, method, end_date, ...){
            val <- value(object, as.character(by), type, end_date=end_date)
            return(val)
          })

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "character", "character", "missing"),
          definition = function(object, by, type, method, end_date, ...){
            if(type=="nominal") {
              return(FEMS::value(FEMS::events(object, by[1], end_date=by[length(by)]), by, "nominal", ...))
            } else if (type %in% c("markToModel","markToMarket")) {
              stop("Need argument 'method' in order to evaluate 'markToModel'-type value!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "timeDate", "character", "ValuationEngine"),
          definition = function(object, by, type, method, end_date, ...){
            val <- value(object, as.character(by), type, method, end_date=end_date)
            return(val)
          })

#' @include Value.R
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("CurrentAccount", "character", "character", "ValuationEngine"),
          definition = function(object, by, type, method, end_date, ...){
            if (type == "nominal") {
              val <- FEMS::value(FEMS::events(object, by[1], object$RiskFactorConnector, end_date=end_date), by, "nominal", method, ...)
            } else if (type %in% c("markToModel", "markToMarket") ) {
              val <- FEMS::value(FEMS::events(object, by[1], object$RiskFactorConnector, end_date=end_date), by, "markToModel", method, ...)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
            return(val)
          })









