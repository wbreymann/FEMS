#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

cycle.to.by <- function(x) {
  x = gsub("+", "", x)
  x = gsub("-", "", x)
  counter = substring(x, 1, nchar(x)-1)
  unit.char = substring(x, nchar(x), nchar(x))
  unit = switch(unit.char,
              Y="years",
              Q="months",
              M="months",
              W="weeks",
              D="days",
              "unknown")
  mult = switch(unit.char,
              Y=1,
              Q=3,
              M=1,
              W=1,
              D=1,
              "unknown")
  return(paste(mult*as.integer(counter), unit, sep=" "))
}

# A modified version of the aggregate method for timeSeries objects.
# It has an additional variable leftopen that allows the user to choose either
# left closed and right open intervals or vice-versa.
.aggregate.timeSeries = function (x, by, FUN, leftopen=FALSE, ...) 
{
  if (!((inherits(by, "timeDate") && x@format != "counts") || 
        (is.numeric(by) && x@format == "counts"))) 
    stop("'by' should be of the same class as 'time(x)'", 
         call. = FALSE)
  shift = leftopen
  Title <- x@title
  Documentation <- x@documentation
  if (is.unsorted(x)) # sorts x if times unsorted
    x <- sort(x)      
  by <- unique(sort(by))  # assures unique entries
  INDEX <- findInterval(x@positions, as.numeric(by, "sec") + 
                          leftopen)  # converts time of x in seconds and adds 1
  INDEX <- INDEX + leftopen
  is.na(INDEX) <- !(INDEX <= length(by)-1+leftopen)
  data <- matrix(apply(getDataPart(x), 2, tapply, INDEX, FUN), 
                 ncol = ncol(x))
  
  rownames(data) <- as.character(by[unique(na.omit(INDEX))])
  colnames(data) <- colnames(x)
  ans <- timeSeries(data, ...)
  ans@title <- Title
  ans@documentation <- Documentation
  ans
}


check.date.format <- function(y, add = TRUE){
  if (add) {
      if (substr(y, nchar(y) - 2, nchar(y)) == "T00") {
        return(y)
      } else { 
        return(paste0(y,"T00"))
      }
  } else {
    if (substr(y, nchar(y) - 2, nchar(y)) == "T00") {
      return(substr(y, 1, nchar(y) - 3))
    } else { 
      return(y)
    }
  }
}


date.conversion.attributes <- function(x){
  # note this is only set up for PAM and ANN

  if ("MaturityDate" %in% names(x)) {
    x$MaturityDate <- check.date.format(x$MaturityDate)
  }
  if ("InitialExchangeDate" %in% names(x)) {
    x$InitialExchangeDate <- check.date.format(x$InitialExchangeDate)
  }
  if ("StatusDate" %in% names(x)) {
    x$StatusDate <- check.date.format(x$StatusDate)
  }
  if ("ContractDealDate" %in% names(x)) {
    x$ContractDealDate <- check.date.format(x$ContractDealDate)
  }
  if ("CapitalizationEndDate" %in% names(x)) {
    x$CapitalizationEndDate <- check.date.format(x$CapitalizationEndDate)
  }
  return(x)
}

get.summary.fields <- function(contract = "NULL"){
  ## NEEDS TO BE WORKED ON!!!
  fields <- c("StatusDate",
              "ContractDealDate",
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate",
              "DayCountConvention",
              "PremiumDiscountAtIED",
              "CycleAnchorDateOfInterestPayment",
              "CycleOfInterestPayment",
              "CycleAnchorDateOfRateReset",
              "CycleOfRateReset",
              "MarketObjectCodeRateReset")
  if (contract == "Ann") {
    fields <- c(fields,
                "CycleAnchorDateOfPrincipalRedemption",
                "CycleOfPrincipalRedemption",
                "NextPrincipalRedemptionPayment",
                "AmortizationDate")
  } else if (contract == "Nam") {
    fields <- c(fields,
                "CycleAnchorDateOfPrincipalRedemption",
                "CycleOfPrincipalRedemption",
                "NextPrincipalRedemptionPayment",
                "CycleAnchorDateOfRateReset",
                "CycleOfRateReset")
  } else if (contract == "Lam") {
    fields <- c(fields,
                "CycleAnchorDateOfPrincipalRedemption",
                "CycleOfPrincipalRedemption",
                "NextPrincipalRedemptionPayment")
  } else if (contract == "Lax") {
    fields <- c(fields,
                "ArrayCycleAnchorDateOfInterestPayment",
                "ArrayCycleOfInterestPayment",
                "ArrayCycleAnchorDateOfRateReset",
                "ArrayCycleOfRateReset",
                "ArrayRate",
                "ArrayFixedVariable",
                "MarketObjectCodeRateReset",
                "ArrayCycleAnchorDateOfPrincipalRedemption",
                "ArrayCycleOfPrincipalRedemption",
                "ArrayNextPrincipalRedemptionPayment",
                "ArrayIncreaseDecrease")
  } else if (contract == "Fxout") {
    ## NEEDS TO BE FIXED
    fields = list(
      StatusDate="character",
      ContractDealDate="character",
      ContractRole="character",
      ContractID="character",          
      Currency="character",
      Currency2="character",
      MaturityDate="character",
      NotionalPrincipal="numeric",
      NotionalPrincipal2="numeric",
      MarketValueObserved="numeric",
      MarketObjectLink = "character")
  } else if (contract == "Futur") {
    ## NEEDS TO BE FIXED
    fields = list(
      ContractDealDate = "character",
      SettlementDate = "character",
      ClearingHouse = "character",
      CycleAnchorDateOfMargining = "character",
      CycleOfMargining = "character",
      ConversionFactor = "numeric",
      InitialMargin = "numeric",
      VariationMargin = "numeric",
      MaintenanceMarginLowerBound = "numeric",
      PriceAtTD = "numeric")
  } else if (contract == "Optns") {
    ## NEEDS TO BE FIXED
    fields = list(
      ContractDealDate = "character",
      SettlementDate = "character",
      ClearingHouse = "character",
      CycleAnchorDateOfMargining = "character",
      CycleOfMargining = "character",
      ConversionFactor = "numeric",
      InitialMargin = "numeric",
      VariationMargin = "numeric",
      MaintenanceMarginLowerBound = "numeric",
      PriceAtTD = "numeric"
    )
  } else if (contract == "Stk") {
    ## NEEDS TO BE FIXED
    fields = list(
      StatusDate="character",
      ContractDealDate="character",
      Notional = "numeric",
      PriceAtTD = "numeric",
      CycleAnchorDateOfDividend = "character",
      CycleOfDividend = "character",
      Quantity = "numeric",
      MarketObjectLink = "character"
    )
  } else if (contract == "Swap") {
    ## NEEDS TO BE FIXED
    fields = list(
      StatusDate = "character",
      ContractDealDate = "character",
      DeliverySettlement = "character",
      PurchaseDate = "character",
      PriceAtPurchaseDate = "numeric",
      TerminationDate = "character",
      PriceAtTerminationDate = "numeric"
    )
  }
  return(fields)
}


# util function for tree aggregation
aggregate.leafs=function(leafs,branches,col.names) {
  out=leafs
  if(length(branches)>0) {
    treemap=unlist(branches)
    names(treemap)=unlist(sapply(names(branches),FUN=function(x) paste(x,1:length(branches[[x]]),sep=".")))
    child=leafs
    idx=treemap%in%names(child)
    namesmap=unlist(lapply(strsplit(names(treemap),".",fixed=TRUE),function(x)x[1]))
    names(namesmap)=treemap
    names(out)=paste(namesmap[names(out)],names(out),sep=":")
    while(sum(idx)>0) {
      parent=as.list(unique(unlist(lapply(strsplit(names(treemap)[idx],".",fixed=TRUE),FUN=function(x)x[1]))))
      temp=lapply(parent,FUN=function(x) {
        apply(as.data.frame(child[treemap[grep(x,names(treemap))]]),1,sum)
      })
      names(temp)=unlist(parent)
      out=c(temp,out)
      treemap=treemap[!idx]
      child=temp
      idx=treemap%in%names(child)
      if(sum(idx)>0) {
        namesmap=unlist(lapply(strsplit(names(treemap),".",fixed=TRUE),function(x)x[1]))
        names(namesmap)=treemap
        names(out)=paste(namesmap[unlist(lapply(strsplit(names(out),":",fixed=TRUE),function(x)x[1]))],names(out),sep=":")
      }
    }
  }
  out=rflPortfolio:::format.tree.results(out,col.names)
  return(out)
}

# util function for tree formatting
format.tree.results=function(x, col.names) {
  out=t(as.data.frame(x))
  out=as.data.frame(out[sort(rownames(out)),])
  colnames(out)=as.character(col.names[-1])
  return(out)
}

## -----------------------------------------------------------------
## private util methods
## get long name for ContractType from short
longName <- function(name) {
  short <- c("pam", "ann", "nam", "lam", "stk",
             "fxout", "swaps", "futur", "optns","lax")
  long <- c("principalatmaturity", "annuity", "negativeamortizer",
            "linearamortizer", "stock", "foreignexchangeoutright",
            "swap", "future", "option","exoticlinearamortizer")
  target <- c("PrincipalAtMaturity", "Annuity", "NegativeAmortizer",
              "LinearAmortizer", "Stock", "ForeignExchangeOutright",
              "Swap", "Future", "Option","ExoticLinearAmortizer")
  names.table <- data.frame(short=short, long=long, target=target)
  if(tolower(name)%in%short) {
    out <- names.table[which(short==tolower(name)), "target"]
  } else if(tolower(name)%in%long) {
    out <- names.table[which(long==tolower(name)), "target"]
  } else {
    stop(paste("ContractType", name, "does not exist!", sep=" "))
  }
  return(out)
}



