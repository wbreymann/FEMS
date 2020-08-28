#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 09.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include cashFlows.R util.R
#' @export 
convexity <- function(x, yield=NULL, price=NULL, 
                      isPercentage=TRUE, per=NULL) {
  
  # if no date provided, we use initial exchange date such that initial cash flow will not be 
  # considered for duration calculation
  if(is.null(per)) {
    per <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  
  # compute cash flows
  cf <- cashFlows(x, per=per)
  
  # number of coupon payments per year
  m <- couponsPerYear(x)
  
  # if necessary compute yield or price
  if(is.null(price)&&is.null(yield)) {
    stop("please provide either 'price' or 'yield' information!")
  }
  if(is.null(yield)) {
    yield <- yield(x, price=price, isPercentage=isPercentage, per=per)
  } else if(isPercentage) {
    yield <- yield/100
  }
  if(is.null(price)) {
    price <- sum(cf$Value/(1+yield/m)^(cf$Time))
  } else if(isPercentage) {
    price <- as.numeric(FEMS:::get(x, "NotionalPrincipal"))*price/100
  }
  
  # extract times (in years) from cash flows
  t <- cf$Time
  # compute ytm-discounted cash flows
  p.i <- cf$Value/(1+yield)^t
  # compute convexity
  c <- 1/(price*(1+yield)^2*m^2)*sum((t^2+t)*p.i)
  return(as.numeric(c))
}