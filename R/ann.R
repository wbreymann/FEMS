#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
annuity = function(dealDate="2015-01-01", nominal=1000, coupon=0.05, annuity=100, 
                   annuityFreq="1 year", maturity = NULL, role="long", 
                   variable=FALSE, ...) {

  args <- list(...)
  if(is.null(maturity)) {
    maturity <- NULL
  } else if(!is.null(maturity) && nchar(maturity)<13) {
    maturity <- as.character(timeSequence(timeDate(dealDate), 
                                               by=maturity, length.out=2)[2])
  }
  statusDate <- as.character(timeDate(dealDate)-24*3600)
  contractDealDate <- as.character(timeDate(dealDate)-24*3600)
  initialExchangeDate <- dealDate
  
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
    args[["DayCountConvention"]] <- "AA"
  }
  
  attributes <- list(InitialExchangeDate=initialExchangeDate,
                    StatusDate=statusDate,
                    ContractDealDate=contractDealDate,
                    MaturityDate=maturity,
                    NotionalPrincipal=nominal,
                    NominalInterestRate=coupon,
                    CycleOfPrincipalRedemption=annuityFreq,
                    NextPrincipalRedemptionPayment=annuity,
                    ContractRole=role)
  attributes <- append(attributes, args)
  out <- Ann()
  set(out, what=attributes)
  return(out)
}