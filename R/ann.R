#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************

##############################################################
#' \code{annuity}
#'
#' Constructor method for simple Annuity contracts. If interest payments are set, the 
#' cycle of interest rate payments is the same as the cycle for annuity payments.
#' 
#' @param start a character reflecting a start date of the annuity.
#' 
#' @param nominal a numeric to set the notional principal of the annuity, 
#' default is 0.0.
#'
#' @param ir a numeric to set the nominal interest rate, default is 0.0.
#' 
#' @param annuity a numeric to set the next principal redemption payment.
#'  
#' @param annuityFreq a character reflecting the frequency of redemption payments, 
#' default is "1 year". 
#'  
#' @param maturity a character reflecting the maturity of the annuity, 
#' default is "0 years".
#' 
#' @param role a character reflecting the contract role, default is "long".
#' 
#' @return an Annuity contract with specified attributes. 
#' 
#' @usage annuity(start, nominal, ir, annuity, annuityFreq, maturity, role)
#' 
#' @examples
#' ann <- annuity("2013-12-31", nominal = 50000, ir = 0.02, maturity = "5 years")
#' 
#' @include Annuity.R
#' @export 
annuity <- function(start, nominal=0.0, ir=0.0, annuity=NULL, 
                   annuityFreq="1 year", maturity = "0 years", role="long", 
                   variable.rates=FALSE, ...) {

  if (missing(start)){
    stop("Variable start muss gesetzt werden !!!")
  }
  args <- list(...)
  if(is.null(maturity)) {
    maturity <- NULL
  } else if(!is.null(maturity) && nchar(maturity)<13) {
    maturity <- as.character(timeSequence(timeDate(start), 
                                               by=maturity, length.out=2)[2])
  }
  statusDate <- as.character(timeDate(start)-24*3600)
  contractDealDate <- as.character(timeDate(start)-24*3600)
  initialExchangeDate <- start
  
  ann_fr <- annuityFreq
  if(is.null(annuityFreq)) {
    annuityFreq <- NULL
  } else {
    if(length(grep("y", annuityFreq))>0) {
      annuityFreq <- paste("P",gsub("([0-9]*).*","\\1",annuityFreq), "Y", "L1", sep="")
    } else if(length(grep("q", annuityFreq))>0) {
      annuityFreq <- paste("P",gsub("([0-9]*).*","\\1",annuityFreq), "Q", "L1", sep="")
    } else if(length(grep("m", annuityFreq))>0) {
      annuityFreq <- paste("P",gsub("([0-9]*).*","\\1",annuityFreq), "M", "L1", sep="")
    } else if(length(grep("w", annuityFreq))>0) {
      annuityFreq <- paste("P",gsub("([0-9]*).*","\\1",annuityFreq), "W", "L1", sep="")
    } else if(length(grep("d", annuityFreq))>0) {
      annuityFreq <- paste("P",gsub("([0-9]*).*","\\1",annuityFreq), "D", "L1", sep="")
    }else {
      stop("please provide annuityFreq information in timeSeries 'by' format!")
    }
  }
  
  if(role=="long") {
    role <- "RPA"
  } else {
    role <- "RPL"
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

  if (!(is.null(annuity))) {
    args[["NextPrincipalRedemptionPayment"]] <- annuity
  }
  
  attributes <- list(InitialExchangeDate=initialExchangeDate,
                    StatusDate=statusDate,
                    ContractDealDate=contractDealDate,
                    MaturityDate=maturity,
                    NotionalPrincipal=nominal,
                    NominalInterestRate=ir,
                    CycleOfPrincipalRedemption=annuityFreq,
                    CycleAnchorDateOfPrincipalRedemption = 
                      as.character(timeSequence(initialExchangeDate, 
                                                by=ann_fr, 
                                                length.out=2)[2]),
                    CycleOfInterestPayment=annuityFreq,
                    CycleAnchorDateOfInterestPayment = 
                      as.character(timeSequence(initialExchangeDate, 
                                                by=ann_fr, 
                                                length.out=2)[2]),
                    ContractRole=role)
  attributes <- append(attributes, args)
  out <- Ann()
  set(out, what=attributes)
  return(out)
}