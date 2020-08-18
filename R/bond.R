#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
bond = function(dealDate="2015-01-01", maturity="5 years", nominal=1000, 
                coupon=0.05, couponFreq="1 year", role="long", 
                variable=FALSE, ...) {
  
  args <- list(...)
  if(nchar(maturity)<13) {
    maturity <- as.character(timeSequence(timeDate(dealDate), 
                                               by=maturity, length.out=2)[2])
  }
  statusDate <- as.character(timeDate(dealDate)-24*3600)
  contractDealDate <- as.character(timeDate(dealDate)-24*3600)
  initialExchangeDate <- dealDate
  
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
    args[["DayCountConvention"]] <- "AA"
  }
  
  if(variable) {
    args[["CycleOfRateReset"]] <- couponFreq  
  }
  
  attributes <- list(InitialExchangeDate=initialExchangeDate,
                    StatusDate=statusDate,
                    ContractDealDate=contractDealDate,
                    MaturityDate=maturity,
                    NotionalPrincipal=nominal,
                    NominalInterestRate=coupon,
                    CycleOfInterestPayment=couponFreq,
                    ContractRole=role)
  attributes <- append(attributes, args)
  out <- Pam()
  set(out, what=attributes)
  return(out)
}