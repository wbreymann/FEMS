#*******************************************************************************
# ZHAW Risk and Finance Lab
# Date: 18.08.2020
# IDP - Institute for Data Analysis and Process Design
# author(s): Christoph Auth (auth@zhaw.ch)
#*******************************************************************************

##############################################################
#' \code{loan}
#'
#' Constructor method for simple LinearAmortizer contracts.
#' 
#' @param start a character reflecting the start date of the loan.
#' 
#' @param maturity a character reflecting the maturity of the loan, 
#' default is "0 years".
#' 
#' @param nominal a numeric to set the notional principal of the loan, 
#' default is 0.0.
#'
#' @param ir a numeric to set the nominal interest rate, default is 0.0.
#'
#' @param irFreq a character reflecting the frequency of interest rate payments, 
#' default is "1 year". 
#' 
#' @param role a character reflecting the contract role. default is "long".
#'  
#' @param amortFreq a character reflecting the frequency of principal redemption 
#' payments, default is "1 year". 
#' 
#' @param amort a numeric to set the amount of principal redemption.
#' 
#' @return a LinearAmortizer contrat with specified attributes. 
#' 
#' @usage loan(start, maturity, nominal, ir, irFreq, role, amortFreq, amort)
#' 
#' @examples
#' l <- loan("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            ir = 0.02, irFreq = "1 years", amortFreq = "1 years")
#'            
#' @include LinearAmortizer.R
#' @export 
loan <- function(start, maturity = "0 years", nominal = 0, 
                ir = 0.0, irFreq="1 year", role = "long", 
                amortFreq = "1 year", amort = NULL,
                variable.rates = FALSE, ...) {
  if (missing(start)){
    stop("Variable start muss gesetzt werden !!!")
  }
  args <- list(...)
  if(nchar(maturity)<10) {
    maturity <- as.character(timeSequence(timeDate(start), 
                                          by=maturity, length.out=2)[2])
  }

  statusDate <- as.character(timeDate(start)-24*3600)
  contractDealDate <- as.character(timeDate(start)-24*3600)
  initialExchangeDate <- start
  ir_freq_bef <- irFreq
  if(is.null(irFreq) || irFreq=="NULL") {
    irFreq <- NULL
    ir <- -999999999
  } else {
    if(length(grep("y", irFreq))>0) {
      irFreq <- paste("P",gsub("([0-9]*).*","\\1",irFreq), "Y", "L1", sep="")
    } else if(length(grep("q", irFreq))>0) {
      irFreq <- paste("P",gsub("([0-9]*).*","\\1",irFreq), "Q", "L1", sep="")
    } else if(length(grep("m", irFreq))>0) {
      irFreq <- paste("P",gsub("([0-9]*).*","\\1",irFreq), "M", "L1", sep="")
    } else if(length(grep("w", irFreq))>0) {
      irFreq <- paste("P",gsub("([0-9]*).*","\\1",irFreq), "W", "L1", sep="")
    } else if(length(grep("d", irFreq))>0) {
      irFreq <- paste("P",gsub("([0-9]*).*","\\1",irFreq), "D", "L1", sep="")
    }else {
      stop("please provide irFreq information in timeSeries 'by' format!")
    }
  }
  amort_freq_bef <- amortFreq
  if(is.null(amortFreq) || amortFreq=="NULL") {
    amortFreq <- NULL
  } else {
    if(length(grep("y", amortFreq))>0) {
      amortFreq <- paste("P",gsub("([0-9]*).*","\\1",amortFreq), "Y", "L1", sep="")
    } else if(length(grep("q", amortFreq))>0) {
      amortFreq <- paste("P",gsub("([0-9]*).*","\\1",amortFreq), "Q", "L1", sep="")
    } else if(length(grep("m", amortFreq))>0) {
      amortFreq <- paste("P",gsub("([0-9]*).*","\\1",amortFreq), "M", "L1", sep="")
    } else if(length(grep("w", amortFreq))>0) {
      amortFreq <- paste("P",gsub("([0-9]*).*","\\1",amortFreq), "W", "L1", sep="")
    } else if(length(grep("d", amortFreq))>0) {
      amortFreq <- paste("P",gsub("([0-9]*).*","\\1",amortFreq), "D", "L1", sep="")
    }else {
      stop("please provide irFreq information in timeSeries 'by' format!")
    }
  }
  
  
  if(role=="long") {
    role <- "RPA"
  } else if (role=="short") {
    role <- "RPL"
  } else {
    stop(paste0("Error in FEMS::loan: '", role,
    "' is not a valid name for argument 'role'")
    )
  }
  
  if(!"Currency"%in%names(args)) {
    args[["Currency"]] <- "CHF"
  }
  
  if(!"Calendar"%in%names(args)) {
    args[["Calendar"]] <- "NC"
  }
  
  if(!"DayCountConvention"%in%names(args)) {
    args[["DayCountConvention"]] <- "30E360"
  }
  
  if(variable.rates) {
    args[["CycleOfRateReset"]] <- irFreq  
  }
  if (!(is.null(amort))) {
    args[["NextPrincipalRedemptionPayment"]] <- amort
  }

  attributes <- list(InitialExchangeDate=initialExchangeDate,
                     StatusDate=statusDate,
                     ContractDealDate=contractDealDate,
                     MaturityDate=maturity,
                     NotionalPrincipal=nominal,
                     NominalInterestRate=ir,
                     CycleOfInterestPayment=irFreq,
                     CycleAnchorDateOfInterestPayment = 
                       as.character(timeSequence(initialExchangeDate, 
                                                 by=ir_freq_bef, 
                                                 length.out=2)[2]),
                     ContractRole=role,
                     CycleOfPrincipalRedemption=amortFreq,
                     CycleAnchorDateOfPrincipalRedemption = 
                       as.character(timeSequence(initialExchangeDate, 
                                                 by=amort_freq_bef, 
                                                 length.out=2)[2]))
                     
  attributes <- append(attributes, args)
  out <- Lam()
  set(out, what = attributes)
  return(out)
}