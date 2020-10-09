#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include cashFlows.R presentValue.R yieldToMaturity.R util.R DynamicYieldCurve.R YieldCurve.R
#' @export 
duration <- function(x, type="mac", yield=NULL, yieldCurve=NULL, price=NULL, 
                     isPercentage=TRUE, from=NULL) {

  if(type=="gen"&&is.null(yieldCurve)) {
    stop("for the general duration type, please provide a yield curve!")  
  }
  if(type!="gen"&&is.null(price)&&is.null(yield)) {
    stop("for non-general duration types, please provide either 'price' or 'yield' information!")
  } 
  
  if(class(x)=="Portfolio") {
    cts <- FEMS:::get(x, "contracts")
    if(!is.null(yield) && length(yield)!=length(cts)) {
      stop("please set 'yield=NULL' or provide 'yield' with length same as number of contracts in the Portfolio!")
    }
    if(is.null(yield)) {
      yield <- rep(NULL, length(cts))
    }
    if(is.null(price)) {
      price <- rep(NULL, length(cts))
    }
    d <- numeric(length(cts))
    for(i in 1:length(cts)) {
      d[i] <- duration(cts[[i]], type, yield[i], yieldCurve, price[i], isPercentage, from)
    }
    if(is.null(price[1])) {
      price <- numeric(length(cts))
      for(i in 1:length(cts)) {
        price[i] <- presentValue(cts[[i]], yield[i], yieldCurve, from, isPercentage, isPrice=TRUE)
      }
    }
    return(as.numeric(t(price/sum(price))%*%d))
  }
    
    
    if(type!="gen") {
      if(is.null(yield)) {
        yield <- yield(x, price=price, isPercentage=isPercentage, from=from)
      }
      if(isPercentage) {
        yield <- yield/100
      }
    }
    
    # if no date provided, we use initial exchange date such that initial cash flow will not be 
    # considered for duration calculation
    if(is.null(from)) {
      from <- as.character(FEMS:::get(x,"InitialExchangeDate"))
    }
    
    # compute cash flows
    cf <- cashFlows(x, from=from)
    if ( as.character(time(cf)[1,]) == from )
      cf <- cf[-1,]
    # extract times (in years) from cash flows
    t <- cf$Time
    if(type=="gen") {
      df <- discountFactors(yieldCurve, as.character(time(cf)), isDateEnd=TRUE)
      d <- sum(t*df*cf$Value)/t(cf$Value)%*%df
    } else {
      # get number of coupon periods per year
      m <- couponsPerYear(x)
      # compute (yield-) discounted cash flows
      p.i <- cf$Value/(1+yield)^t
      # compute macaulay duration
      d <- sum(t*p.i)/sum(p.i)
      # scale appropriately if modified or dollar duration
      if(type!="mac") {
        d <- d/(1+yield/m)
      }
      if(type=="dol") {
        d <- d*sum(p.i)/100
      }
    }
    return(as.numeric(d))
  }