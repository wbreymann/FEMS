#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************

##############################################################
#' \code{yield}
#'
#' Function which calculates the yield to maturity (YTM) for passed
#' contract types.
#' 
#' @param x a contract type, for which to calculate the YTM.
#' 
#' @param price a numeric, indicating the price used for calculating the YTM.
#'  
#' @param isPercentage a logical, indicating if the 'price' is inserted as percentage or not.
#'                     (default is TRUE). 
#'                     
#' @param from a character indicating the date as for which the YTM is calculated.
#' 
#' @return a numeric, representing the yield to maturity (YTM) of the contract. 
#' 
#' @usage yield(x, price, isPercentage, from)
#' 
#' @examples
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            coupon = 0.02, couponFreq = "1 years")
#' ytm <- yield(b, price = 100) 
#' 
#' @include cashFlows.R util.R
#' @export 
yield <- function(x, price, isPercentage=TRUE, from=NULL) {
  
  # date as from which 'price' is valid 
  # (and hence, as from which we calculate yield)
  if(is.null(from)) {
    from <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  
  # compute cash flows of bond instrument
  cf <- cashFlows(x, from=from)
  
  # scale factor for annualized yield (i.e. number of coupons from year)
  scale <- couponsPerYear(x)
  
  if(isPercentage) {
    absPrice <- price/100* as.numeric(FEMS:::get(x,"NotionalPrincipal"))
  } else {
    absPrice <- price
  }
  
  # target function
  f <- function(lambda) {
    absPrice-sum(cf$Value/(1+lambda*scale)^(cf$Time))
  }
  
  # numerical procedure to find unit-root
  res <- uniroot(f=f,interval=c(-0.99,0.99))
  
  # return unit-root which is yield
  return(res$root)
}