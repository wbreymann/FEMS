#*******************************************************************************
# ZHAW Risk and Finance Lab
# Date: 18.08.2020
# IDP - Institute for Data Analysis and Process Design
# author(s): Christoph Auth (auth@zhaw.ch)
#*******************************************************************************
credit = function(dealDate="2015-01-01", maturity="5 years", nominal=1000, 
                coupon=0.05, couponFreq="1 year", role="long", 
                redemptionFreq="1 year", redemption=200,
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
  
  if(is.null(redemptionFreq) || redemptionFreq=="NULL") {
    redemptionFreq <- NULL
  } else {
    if(length(grep("y", redemptionFreq))>0) {
      redemptionFreq <- paste("P",gsub("([0-9]*).*","\\1",redemptionFreq), "Y", "L1", sep="")
    } else if(length(grep("q", redemptionFreq))>0) {
      redemptionFreq <- paste("P",gsub("([0-9]*).*","\\1",redemptionFreq), "Q", "L1", sep="")
    } else if(length(grep("m", redemptionFreq))>0) {
      redemptionFreq <- paste("P",gsub("([0-9]*).*","\\1",redemptionFreq), "M", "L1", sep="")
    } else if(length(grep("w", redemptionFreq))>0) {
      redemptionFreq <- paste("P",gsub("([0-9]*).*","\\1",redemptionFreq), "W", "L1", sep="")
    } else if(length(grep("d", redemptionFreq))>0) {
      redemptionFreq <- paste("P",gsub("([0-9]*).*","\\1",redemptionFreq), "D", "L1", sep="")
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
                     ContractRole=role,
                     CycleOfPrincipalRedemption=redemptionFreq,
                     NextPrincipalRedemptionPayment=redemption)
  attributes <- append(attributes, args)
  out <- Lam()
  set(out, what=attributes)
  return(out)
}