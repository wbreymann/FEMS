#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include PrincipalAtMaturity.R
#' @export 
bond <- function(start, maturity="0 years", nominal=0, 
                coupon=0.0, couponFreq="1 year", role="long", 
                variable.rates=FALSE, ...) {
  if (missing(start)){
    stop("Variable start muss gesetzt werden !!!")
  }
  args <- list(...)
  if(nchar(maturity)<13) {
    maturity <- as.character(timeSequence(timeDate(start), 
                                               by=maturity, length.out=2)[2])
  }
  statusDate <- as.character(timeDate(start)-24*3600)
  contractDealDate <- as.character(timeDate(start)-24*3600)
  initialExchangeDate <- start
  
  couponFreq_bef <- couponFreq
  if(is.null(couponFreq) || couponFreq=="NULL") {
    couponFreq <- NULL
    coupon <- -999999999
  } else {
    if(length(grep("y", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "Y", "L1", sep="")
    } else if(length(grep("q", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "Q", "L1", sep="")
    } else if(length(grep("m", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "M", "L1", sep="")
    } else if(length(grep("w", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "W", "L1", sep="")
    } else if(length(grep("d", couponFreq))>0) {
      couponFreq <- paste("P",gsub("([0-9]*).*","\\1",couponFreq), "D", "L1", sep="")
    }else {
      stop("please provide couponFreq information in timeSeries 'by' format!")
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
  
  if(variable.rates) {
    args[["CycleOfRateReset"]] <- couponFreq  
  }
  
  attributes <- list(InitialExchangeDate=initialExchangeDate,
                    StatusDate=statusDate,
                    ContractDealDate=contractDealDate,
                    MaturityDate=maturity,
                    NotionalPrincipal=nominal,
                    NominalInterestRate=coupon,
                    CycleOfInterestPayment=couponFreq,
                    CycleAnchorDateOfInterestPayment = 
                      as.character(timeSequence(initialExchangeDate, 
                                                by=couponFreq_bef, 
                                                length.out=2)[2]),
                    ContractRole=role)
  attributes <- append(attributes, args)
  out <- Pam()
  set(out, what=attributes)
  return(out)
}