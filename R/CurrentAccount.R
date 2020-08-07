#' YieldCurve2.R
#' @export
setRefClass("CurrentAccount",
            fields = list(
              ContractID = "character",
              ContractType = "character",
              ContractDealDate = "character",
              EndDate = "character",
              Currency = "character",
              Inflows = "data.frame",
              Outflows = "data.frame",
              PercentageOutflows = "data.frame",
              CycleAnchorDateOfInterestPayment = "character",
              CycleOfInterestPayment = "character",
              YieldCurve = "YieldCurve2"
            ))

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
                         Inflows=data.frame(),
                         Outflows=data.frame(),
                         PercentageOutflows=data.frame())
            if(length(pars)==0){
            }  else if (is.list(pars[[1]])) {
              rFEMS:::set(object=object, what=pars[[1]])
            } else {
              rFEMS:::set(object=object, what=pars)
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
            if(what=="all") what=rFEMS:::terms(object)
            fields = sapply(what,function(x) object$field(x))
            return(fields)
          })

#' @include Events.R
#' @export
setMethod(f = "events", signature = c("CurrentAccount", "character", "missing"),
          definition = function(object, ad, model){
            return(rFEMS:::EventSeries(object,ad))
          })

#' @export
setMethod(f = "EventSeries", signature = c("CurrentAccount", "character"),
          definition = function(object, ad, ...){
            
            # create event series object
            out <- new("EventSeries")
            out$id <- rFEMS:::get(object,"ContractID")
            out$ct <- rFEMS:::get(object,"ContractType")
            
            # evaluate reserving pattern
            out$evs <- currentaccount.evs2(object, method="compound", period="Y")
            return(out)
          })

currentaccount.evs2 <- function(object, method, period){
  
  interest_dates <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, 
                                         object$CycleOfInterestPayment, object$EndDate)
  all_dates <- sort(unique(c(interest_dates,rownames(object$Inflows),
                             rownames(object$Outflows),rownames(object$PercentageOutflows))))
  if (min(all_dates) < object$YieldCurve$ReferenceDate[1]) {
    stop("ErrorIn::CurrentAccount:: Dates must all lay after first ReferenceDate of YieldCurve !!! ")
  }
  
  # expand all data.frames to reflect same dates...
  inflow_df <- rbind(object$Inflows, 
                     setNames(data.frame(rep(0,length(all_dates[!is.element(all_dates,rownames(object$Inflows))])), 
                   row.names=all_dates[!is.element(all_dates,rownames(object$Inflows))]),
                   names(object$Inflows)))
  
  inflow_df <- inflow_df[order(row.names(inflow_df)),,drop=FALSE]
  outflow_df <- rbind(object$Outflows, 
                      setNames(data.frame(rep(0,length(all_dates[!is.element(all_dates,rownames(object$Outflows))])), 
                                row.names=all_dates[!is.element(all_dates,rownames(object$Outflows))]),
                               names(object$Outflows)))
  outflow_df <- outflow_df[order(row.names(outflow_df)),,drop=FALSE]
  perc_outflow_df <- rbind(object$PercentageOutflows, 
                           setNames(data.frame(rep(0,length(all_dates[!is.element(all_dates,rownames(object$PercentageOutflows))])), 
                                 row.names=all_dates[!is.element(all_dates,rownames(object$PercentageOutflows))]),
                                 names(object$PercentageOutflows)))
  perc_outflow_df <- perc_outflow_df[order(row.names(perc_outflow_df)),,drop=FALSE]
  
  # neeed to create IP, IF, OF, 
  current_account_bef <- 0
  current_account <- 0
  outflow <- 0
  for (i in 1:length(all_dates)) {
    if (i==1){
      current_account_bef <- inflow_df[i,]
      outflow <- -current_account_bef*perc_outflow_df[i,] - outflow_df[i,]
      current_account <- current_account_bef + outflow
    } else {
      df_s <- discountFactorsv2(object$YieldCurve, all_dates[i-1], all_dates[i], method=method, period=period)
      current_account_bef <- c(current_account_bef, df_s*current_account[i-1] + inflow_df[i,]) 
      outflow <- c(outflow, -current_account_bef[i]*perc_outflow_df[i,] - outflow_df[i,])
      current_account <- c(current_account, current_account_bef[i] + outflow[i])
    }
  }
  out <- data.frame(CurrentAccount = current_account, Inflows = inflow_df[,], Outflows = outflow)
  rownames(out) <- all_dates
  return(out)
}

currentaccount.evs <- function(object, method, period){
  browser()
  dates <- get.dates.from.cycle(object$CycleAnchorDateOfInterestPayment, object$CycleOfInterestPayment, object$EndDate)
  if (min(dates) < object$YieldCurve$ReferenceDate[1]){
    stop("ErrorIn::YieldCurve2::cashflows:: Dates must all lay after first ReferenceDate of YieldCurve !!! ")
  }
  for (i in 1:length(dates)) {
    if (i==1){
      #df_s <- discountFactorsv2(yc, yc$ReferenceDate[1], dates[i], method=method, period=period)
      balance <- object$NotionalPrincipal
      payout <- -object$NotionalPrincipal*object$Outflow
    } else {
      df_s <- discountFactorsv2(object$YieldCurve, dates[i-1], dates[i], method=method, period=period)
      balance <- c(balance,df_s*balance[i-1])
      if (i == length(dates)) {
        payout <- c(payout,df_s*payout[i-1])
      } else {
        payout <- c(payout,df_s*payout[i-1]-(balance[i]+payout[i-1]*df_s)*object$Outflow)
      }
      
    }
  }
  out <- data.frame(Balance = balance, Payout = payout, Total = payout+balance)
  rownames(out) <- dates
  return(out)
}

get.dates.from.cycle <- function(anchor_date, cycle, end_date){
  # Note that this does not fit to the the desired daycount convention yet...
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  tSeq <- timeSequence(anchor_date, end_date, by = by)
  return(as.character(tSeq))
}






