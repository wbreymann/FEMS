#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************

##############################################################
#' \code{presentValue}
#'
#' Function which calculates the Net Present Value (NPV) for passed
#' contract types.
#' 
#' @param x a contract type, for which to calculate the NPV.
#' 
#' @param yield a numeric, indicating the percentage yield used to discount.
#' 
#' @param yieldCurve an object of type \code{\link{YieldCurve}} or 
#'                    \code{\link{DynamicYieldcurve}} to calculate discount 
#'                    factors from.
#'
#' @param from a character indicating the date as for which the NPV is calculated.
#'  
#' @param isPercentage a logical, indicating if the 'yield' is inserted as percentage or not.
#'                     (default is TRUE). 
#' 
#' @return a numeric, representing the Net Present Value (NPV) of the contract. 
#' 
#' @usage presentValue(x, yield, yieldCurve, from, isPercentage)
#' 
#' @examples
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            coupon = 0.02, couponFreq = "1 years")
#' npv <- presentValue(b, yield = 2) # result: 0 due to same coupon as yield
#' 
#' @include cashFlows.R DynamicYieldCurve.R YieldCurve.R
#' @export 

presentValue <- function(x, yield=NULL, yieldCurve=NULL, from=NULL, isPercentage=TRUE) {

  if(is.null(yield) && is.null(yieldCurve)) {
    stop("please provide either yield or yieldCurve to compute the present value!")
  }
  
  if(class(x)=="Portfolio") {
    cts <- FEMS:::get(x, "contracts")
    if(!is.null(yield) && length(yield)!=length(cts)) {
      stop("please set 'yield=NULL' or provide 'yield' with lenght same as number of contracts in the Portfolio!")
    }
    if(is.null(yield)) {
      yield <- rep(NULL, length(cts))
    }
    pv <- 0
    for(i in 1:length(cts)) pv <- 
      pv + presentValue(cts[[i]], yield[i], yieldCurve, from, isPercentage)
    return(pv)
  }
  
  # date as from which 'yieldCurve' is valid 
  # (and hence, as from which we calculate present value)
  if(is.null(from)) {
    from <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  
  # compute cash flows of instrument
  cf <- cashFlows(x, from=from)
  
  # compute discount factors for cash flow dates
  if(is.null(yield)) {
    df <- discountFactors(yieldCurve, paste(as.character(time(cf)), "T00", sep=""),isDateEnd=TRUE)
  } else {
    scale <- 1
    if(isPercentage) {
      scale <- 1/100
    }
    df <- (1+yield*scale)^(-cf$Time)
  }
  
  # compute and return present value
  return (as.numeric(t(cf$Value)%*%df))
}