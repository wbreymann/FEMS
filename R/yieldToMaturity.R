#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include cashFlows.R
#' @export 
yield <- function(x, price, isPercentage=TRUE, per=NULL) {
  
  # date as per which 'price' is valid 
  # (and hence, as per which we calculate yield)
  if(is.null(per)) {
    per <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  
  # compute cash flows of bond instrument
  cf <- cashFlows(x,per=per)
  
  # scale factor for annualized yield (i.e. number of coupons per year)
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