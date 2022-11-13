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


couponsPerYear <- function(x, isContract=TRUE) {

  couponFreq <- x
  if(isContract) {
    if(class(x)=="PrincipalAtMaturity") {
      couponFreq <- FEMS:::get(x,"CycleOfInterestPayment")
    } else {
      couponFreq <- FEMS:::get(x,"CycleOfPrincipalRedemption")
    }
  }
  x <- substring(couponFreq, 3, 3)
  
  m <- switch(x,
           Y=1/as.numeric(gsub("([0-9]*).*","\\1",substring(couponFreq, 2, 3))),
           Q=4/as.numeric(gsub("([0-9]*).*","\\1",substring(couponFreq, 2, 3))),
           M=12/as.numeric(gsub("([0-9]*).*","\\1",substring(couponFreq, 2, 3))),
           W=52/as.numeric(gsub("([0-9]*).*","\\1",substring(couponFreq, 2, 3))),
           D=365/as.numeric(gsub("([0-9]*).*","\\1",substring(couponFreq, 2, 3))),
           1)
  return(m)
}

# -----------------------------------------------------------------
# private util methods
# get rates from YieldCurve for rate reset schedule 
get.data.rate.reset <-  function(yc, anchor_dt, cycle, end_dt, ISO = TRUE){

  if (end_dt < anchor_dt) {
    return(NULL)
  }
  # print(convert.Duration(cycle, ISO))
  times <- as.character(timeSequence(
    from = anchor_dt,  
    to = timeSequence(end_dt, by = convert.Duration(cycle, ISO), length.out = 2)[2],
    by = convert.Duration(cycle, ISO)))
  if (class(yc) == "YieldCurve" || class(yc) == "DynamicYieldCurve") {
    data <- getRateAt(yc, times[2:length(times)], times[1:length(times)-1])
  } else {
    data <- rep(yc, length(times)-1)
  }
  df <- timeSeries(data = data,
                   charvec = times[1:length(times)-1],
                   units = "Values")
  return(df)
}

convert.cycle <- function(cycle) {
  period <- substr(cycle, nchar(cycle)-1, nchar(cycle)-1)
  possible_periods <- c("day", "week", "month", "quarter", "year")
  names(possible_periods) <- c("D", "W", "M", "Q", "Y")
  by <- paste0(substr(cycle, 1, nchar(cycle)-2)," ",possible_periods[[period]])
  return(by)
}

convert.Duration <- function(duration, ISO) {
  
  if (ISO){
    # currently, just take the 2nd and 3rd element
    n_units <- substr(duration, 2, 2)
    units <- substr(duration, 3, 3)
    
    conv_units <- switch(units, "Y" = "years", 
                         "Q" = "quarter", 
                         "M" = "months", 
                         "D" = "days")
    out <- paste(n_units,conv_units)
  } else {
    out <- convert.cycle(duration)
  }
  return(out)
}

get.dates.from.cycle <- function(anchor_date, cycle, end_date){
  tSeq <- timeSequence(anchor_date, end_date, by = convert.cycle(cycle))
  return(as.character(tSeq))
}

##
# A version of converting tenors to dates that avoids "switch"
# However, there is no run time gain

# helper functions
quarters <- function(x) {3*months(x)}
halfyear <- function(x) {6*months(x)}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# vector of function names to be used
dayCountFcts <- c("days", "weeks", "months", "quarters", "halfyear", "years")
names(dayCountFcts) <- c("D", "W", "M", "Q", "H", "Y")

# convert character terms to dates relative to a refDate
tenors2dates2 <- function(refDate, tenors, frame=FALSE){
  
  units <- substrRight(tenors, 1)
  counts <- gsub('.{1}$', '', tenors)
  ll <- as.list(data.frame(rbind(units,as.character(counts))))
  
  fct <- function(x, refDate) {
    f <- dayCountFcts[x[1]]
    n <- list(as.numeric(x[2]))
    as.character(ymd(refDate) %m+% do.call(f, n) )
  }
  
  relativeDates <- as.matrix(as.data.frame(lapply(ll, fct, refDate)))
  dimnames(relativeDates) <- list(refDate, tenors)
  
  if (!frame) {
    if (nrow(relativeDates)>1) {
      stop("ErrorIn::tenors2dates:: If function should return array, only one reference date is allowed !!!")
    }
    out <- c(relativeDates)
  } else {
    out <- data.frame(relativeDates)
    colnames(out) <- dimnames(relativeDates)[[2]]
  }
  return(out)
}

shiftDates2 <- function(dates, shift) {
  # browser()
  # dates is a vector and shift is a scalar
  units <- substrRight(shift, 1)
  counts <- gsub('.{1}$', '', shift)
  dCF <- dayCountFcts[units]
  counts <- as.numeric(counts)
  out = character()
  for (i in 1:length(units)) {
    out <- rbind(out,
      as.character(ymd(dates) %m+% do.call(dCF[i],  list(counts[i])))
    )
  }
  if (nrow(out)==1 || ncol(out)==1 ) {
    out <- as.character(out)
  }
  out
}

