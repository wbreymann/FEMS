#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Christoph Auth (auth@zhaw.ch)
#*******************************************************************************
#' @include CurrentAccount.R
#' @export 
bankAccount = function(dealDate, balance = 0, accrued = 0, 
                       ir = 0.0, irFreq = "1 year", ext_transactions = NULL, 
                       int_transfers = NULL, perc_outflows = NULL, 
                       variable.rates = FALSE, ...) {
  
  ir_freq_bef <- irFreq
  if(is.null(irFreq)) {
    irFreq <- "NULL"
  } else {
    if(length(grep("y", irFreq))>0) {
      irFreq <- paste(gsub("([0-9]*).*","\\1",irFreq), "Y", "-", sep="")
    } else if(length(grep("q", irFreq))>0) {
      irFreq <- paste(gsub("([0-9]*).*","\\1",irFreq), "Q", "-", sep="")
    } else if(length(grep("m", irFreq))>0) {
      irFreq <- paste(gsub("([0-9]*).*","\\1",irFreq), "M", "-", sep="")
    } else if(length(grep("w", irFreq))>0) {
      irFreq <- paste(gsub("([0-9]*).*","\\1",irFreq), "W", "-", sep="")
    } else if(length(grep("d", irFreq))>0) {
      irFreq <- paste(gsub("([0-9]*).*","\\1",irFreq), "D", "-", sep="")
    }else {
      stop("please provide irFreq information in timeSeries 'by' format!")
    }
  }
  
  args <- list(...)
  if (!is.null(ext_transactions)) {
    args[["ExternalTransactions"]] <- ext_transactions
  }
  if (!is.null(int_transfers)) {
    args[["InternalTransfers"]] <- int_transfers
  }
  if (!is.null(perc_outflows)) {
    args[["PercentageOutflows"]] <- perc_outflows
  }
  if(!"Currency" %in% names(args)) {
    args[["Currency"]] <- "CHF"
  }
  ip_anchor <- as.character(timeSequence(dealDate, 
                                         by = ir_freq_bef, 
                                         length.out = 2)[2])
  if(variable.rates) {
    args[["CycleOfRateReset"]] <- irFreq  
    args[["CycleAnchorDateOfRateReset"]] <- ip_anchor  
  }
  out <- CurrentAccount(ContractID = "Account",
                        ContractDealDate = dealDate,
                        Balance = balance,
                        AccruedInterest = accrued,
                        NominalInterestRate = ir,
                        CycleAnchorDateOfInterestPayment = ip_anchor,
                        CycleOfInterestPayment = irFreq)
  set(out, args)
  return(out)
  
}