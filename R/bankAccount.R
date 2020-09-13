##############################################################
#' \code{bankAccount}
#'
#' Function which creates an instance of \code{CurrentAccount}.
#' 
#' @param start Start date of the account
#' 
#' @param balance Initial balance of the account
#'
#' @param accrued Accrued interest up to the start date
#' 
#' @param ir Interest rate
#' 
#' @param irFreq Interest rate frequency
#' 
#' @param ext_transactions timeSeries object indicating ExternalTransactions
#' 
#' @param int_transfers timeSeries object indicating InternalTransfers
#' 
#' @param perc_outflows timeSeries object indicating PercentageOutflows
#' 
#' @param currency Currency of the account
#' 
#' @param variable.rates Logical, if the account has variable rates.
#' 
#' @return An object of class \code{CurrentAccount} 
#' 
#' @examples
#' cashflows <- timeSeries(c(1000,1000,30000,10000,-20000), units="CHF", 
#'                 timeSequence(from="2014-04-01", by="year", length.out=5))
#' my.account <- bankAccount("2013-12-31", balance=50000, 
#'                          ext_transactions = cashflows, ir=0.02)
#' 
#' @include CurrentAccount.R
#' @export 
bankAccount = function(start, balance = 0, accrued = 0, 
                       ir = 0.0, irFreq = "1 year", ext_transactions = NULL, 
                       int_transfers = NULL, perc_outflows = NULL, currency = "CHF", 
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

  ip_anchor <- as.character(timeSequence(start, 
                                         by = ir_freq_bef, 
                                         length.out = 2)[2])
  if(variable.rates) {
    args[["CycleOfRateReset"]] <- irFreq  
    args[["CycleAnchorDateOfRateReset"]] <- ip_anchor  
  }
  out <- CurrentAccount(ContractID = "Account",
                        ContractDealDate = start,
                        Balance = balance,
                        AccruedInterest = accrued,
                        NominalInterestRate = ir,
                        CycleAnchorDateOfInterestPayment = ip_anchor,
                        CycleOfInterestPayment = irFreq,
                        Currency = currency)
  set(out, args)
  return(out)
  
}