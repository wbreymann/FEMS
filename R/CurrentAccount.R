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
              # Zusätzliche Variablen (sind Statusvariablen des Contracts)
              # Sie werden benötigt, um den Kontostand und die aufgelaufenen
              # Zinsen nachzuführen. 
              # Die Werte von NominalPrincipal und AccruedInterest sind immer
              # per StatusDate
              # Sie sollten auch bei der Kontraktinitialisierung auf einen
              # vom Benutzer zu setzenden Startwert gesetzt werden.
              StatusDate = "timeDate",
              NominalPrincipal = "numeric",
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
                         Period="Y")
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
            out$evs[rownames(out$evs)>=ad,] # inefficient, should be done in function call...
            return(out)
          })

currentaccount.evs <- function(object, model, end_date, method, period){
  
  yc <- get(model, object$MarketObjectCodeRateReset)
  interest_dates <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, 
                                         object$CycleOfInterestPayment, end_date)
  all_dates <- sort(unique(c(interest_dates,
                             rownames(object$CashFlows),
                             rownames(object$PercentageOutflows),
                             end_date)))
  if (min(all_dates) < yc$ReferenceDate[1]) {
    stop("ErrorIn::CurrentAccount:: Dates must all lay after first ReferenceDate of YieldCurve !!! ")
  }
  
  # expand all data.frames to reflect same dates...
  # Not necessary to do this with data.frames. Possibly switch to array later...
  cashflow_df <- rbind(
    object$CashFlows, 
    setNames(data.frame(
      rep(0,length(all_dates[!is.element(all_dates,rownames(object$CashFlows))])), 
      row.names=all_dates[!is.element(all_dates,rownames(object$CashFlows))]),
      names(object$CashFlows))
    )
  cashflow_df <- cashflow_df[order(row.names(cashflow_df)),,drop=FALSE]
  perc_outflow_df <- rbind(
    object$PercentageOutflows, 
    setNames(data.frame(
      rep(0,length(all_dates[!is.element(all_dates,rownames(object$PercentageOutflows))])), 
      row.names=all_dates[!is.element(all_dates,rownames(object$PercentageOutflows))]),
      names(object$PercentageOutflows)))
  perc_outflow_df <- perc_outflow_df[order(row.names(perc_outflow_df)),,drop=FALSE]
  
  current_account_bef <- 0
  current_account <- 0
  outflow <- 0
  for (i in 1:length(all_dates)) {
    if (i==1){
      current_account_bef <- cashflow_df[i,]
      outflow <- -current_account_bef*perc_outflow_df[i,]
      current_account <- current_account_bef + outflow
    } else {
      df_s <- discountFactorsv2(yc, all_dates[i-1], all_dates[i], method=method, period=period)
      current_account_bef <- c(current_account_bef, df_s*current_account[i-1] + cashflow_df[i,]) 
      outflow <- c(outflow, -current_account_bef[i]*perc_outflow_df[i,])
      current_account <- c(current_account, current_account_bef[i] + outflow[i])
    }
  }
  out <- data.frame(CurrentAccount = current_account, CashFlows = cashflow_df[,], Outflows = outflow)
  rownames(out) <- all_dates
  return(reformat.evs(object, out))
}

get.dates.from.cycle <- function(anchor_date, cycle, end_date){
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  tSeq <- timeSequence(anchor_date, end_date, by = by)
  return(as.character(tSeq))
}

reformat.evs <- function(object, df) {
  df_out <- data.frame(Date = rownames(df),
                       Value = df$CurrentAccount,
                       Type = "CA",
                       Currency = object$Currency,
                       Time = 0,
                       NominalValue = 0,
                       NominalRate = 0,
                       NominalAccrued = 0)
  return(df_out)
}
  






