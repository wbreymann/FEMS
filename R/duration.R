#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' \code{duration}
#'
#' Function which calculates the duration of a portfolio or a contract.
#' 
#' @param x the contract or portfolio of contracts for which to calculate 
#'          the duration.
#' 
#' @param type a \code{character} defining the type of duration; possible types are 
#'             'fisher-weil', 'macaulay' (default) and 'dollar'. 
#'
#' @param yield a \code{numeric}, indicating the yield used to calculate the duration.
#' 
#' @param yieldCurve Object of class \code{YieldCurve} or 
#'                    \code{DynamicYieldCurve} that describes the spot rate term structure.
#'  
#' @param price a \code{numeric}, indicating the price used for calculating the yield to maturity
#'              of each contract.
#' 
#' @param isPercentage a \code{numeric}, indicating if the 'yield' is passed as percentage 
#'                     (TRUE) or as fraction (FALSE). 
#' 
#' @param from a \code{character} indicating the date as for which the net present value
#'             is calculated.
#' 
#' @return a \code{numeic} containing the calculated duration.  
#' 
#' @usage duration(x, type="macaulay", yield, yieldCurve, price, isPercentage=TRUE, from)
#' 
#' @details 
#' For the Macaulay duration, if \code{yield} is not provided, \code{price} should
#' be provided and is used to calculate the  \code{yield}.
#' For the Fisher-Weil duration, \code{yieldCurve} must be specified. In this
#' case the argument \code{price} has no effect. 
#' 
#' 
#' @examples
#' bnd1=bond(start="2015-01-01", maturity="30 years", nominal=1000, 
#'           coupon=0.06, couponFreq="1 year", role="long", variable=FALSE)
#' duration(bnd1, type="macaulay", yield=9)
#' 
#' @include cashFlows.R presentValue.R yieldToMaturity.R util.R DynamicYieldCurve.R YieldCurve.R
#' @export 
duration <- function(x, type="macaulay", yield=NULL, yieldCurve=NULL, price=NULL, 
                     isPercentage=TRUE, from=NULL, digits=2) {

  if(type=="fisher-weil"&&is.null(yieldCurve)) {
    stop("for the general duration type, please provide a yield curve!")  
  } else {
    if(is.null(price)&&is.null(yield)) {
      stop("for non-general duration types, please provide either 'price' or 'yield' information!")
    } 
    if ( !is.element(type, c("macaulay","dollar")) ) {
      stop("Duration type not known.")
    }
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
        price[i] <- presentValue(cts[[i]], yield[i], yieldCurve, from, isPercentage, 
                                 isPrice=TRUE, digits=digits+2)
      }
    }
    return(round(as.numeric(t(price/sum(price))%*%d),digits))
  }
    
    
    if(type!="fisher-weil") {
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
    if(type=="fisher-weil") {
      df <- discountFactors(yieldCurve, to=as.character(time(cf)))
      d <- sum(t*df*cf$Value)/t(cf$Value)%*%df
    } else {
      # get number of coupon periods per year
      m <- couponsPerYear(x)
      # compute (yield-) discounted cash flows
      p.i <- cf$Value/(1+yield)^t
      # compute macaulay duration
      d <- sum(t*p.i)/sum(p.i)
      # scale appropriately if modified or dollar duration
      if(type!="macaulay") {
        d <- d/(1+yield/m)
      }
      if(type=="dollar") {
        d <- d*sum(p.i)/100
      }
    }
    return(round(as.numeric(d), digits))
  }
