#' @export
setRefClass("CurrentAccount",
            fields = list(
              ContractID = "character",
              ContractType = "character",
              ContractDealDate = "character",
              Currency = "character",
              CashFlows = "data.frame",
              PercentageOutflows = "data.frame", # besser timeSeries?
              CycleAnchorDateOfInterestPayment = "character",
              CycleOfInterestPayment = "character",
              MarketObjectCodeRateReset = "character",
              Compound = "character",
              Period = "character",
              StatusDate = "timeDate",
              NotionalPrincipal = "numeric",
              AccruedInterest = "numeric"
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

#' @export
setMethod(f = "get", signature = "CurrentAccount",
          function(object, what, ...){
            # currently not working to return data.frames
            fields <- sapply(what,function(x) object$field(x))
            return(fields)
          })

#' @include Events.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "RiskFactorConnector"),
          definition = function(object, ad, model, end_date){
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

get.dates.from.cycle <- function(anchor_date, cycle, end_date){
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  tSeq <- timeSequence(anchor_date, end_date, by = by)
  return(as.character(tSeq))
}







