#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 10.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' \code{immunize}
#'
#' Immunization minimizes the interest rate risk of a fixed-income portfolio by adjusting the 
#' portfolio duration such that it matches the duration of the liabilities that
#' should be covered with this portfolio. 
#' 
#' @param x the portfolio of contracts that should be adjusted.
#' 
#' @param target the liabilities (contract or portfolio) whose value and duration
#' should be matched.
#'
#' @param yield a numeric, indicating the yield used to calculate the duration.
#' 
#' @param isPercentage a logical, indicating if the 'yield' is expressed as percentage 
#'                     (TRUE) or as fraction (FALSE).
#'  
#' @param period argument currently not used.
#' 
#' @param ... additional parameters to be passed. 
#' 
#' @return the immunization of the contract or portfolio with respect to the target.
#' 
#' @usage immunize(x, target, yield, isPercentage=TRUE, period, ...)
#' 
#' @examples
#' bnd1 <- bond(start="2015-01-01", maturity="30 years", nominal=1000, 
#'           coupon=0.06, couponFreq="1 year", role="long", variable=FALSE)
#' bnd2 <- bond(start="2015-01-01", maturity="10 years", nominal=1000, 
#'           coupon=0.11, couponFreq="1 year", role="long", variable=FALSE)
#' bnd3 <- bond(start="2015-01-01", maturity="20 years", nominal=1000, 
#'           coupon=0.09, couponFreq="1 year", role="long", variable=FALSE)
#'           
#' ptf <- Portfolio(b1=bnd1,b2=bnd2,b3=bnd3)
#' 
#' target <- bond(start="2015-01-01", maturity="10 years", nominal=1000000, coupon=0, 
#'                couponFreq="10 year", role="long", variable=FALSE)
#' immunize(ptf, target, yield=9, from=NULL)
#' 
#' @include duration.R presentValue.R 
#' @export 
immunize <- function(x, target, yield, isPercentage=TRUE, period=NULL, type="macauley", ...) {

  target.val <- presentValue(target, yield=yield, isPercentage=isPercentage, 
                             isPrice=TRUE, ...)
  target.dur <- duration(target, yield=yield, isPercentage=isPercentage, ...)

  cts <- FEMS:::get(x, "contracts")
  
  durations <- numeric(length(cts))
  for(i in 1:length(cts)) {
    durations[i] <- duration(cts[[i]], yield=yield, yieldCurve=NULL, 
                          price=NULL, isPercentage=isPercentage, type)
  }
  
  if(max(range(durations))<target.dur || min(range(durations))>target.dur) {
    stop("single bond durations do not support immunization w.r.t. the target duration!")
  }
  
  price <- numeric(length(cts))
  for(i in 1:length(cts)) {
    price[i] <- presentValue(cts[[i]], yield, yieldCurve=NULL, isPercentage, isPrice=TRUE, ...)
  }
  
  d.low <- which(durations<target.dur, arr.ind=TRUE)
  d.hig <- which(durations>target.dur, arr.ind=TRUE)
  bnd.pairs <- expand.grid(d.low,d.hig) 
  
  res <- list()
  for(i in 1:nrow(bnd.pairs)) {
    pair <- as.numeric(bnd.pairs[i,])
    v1 <- (target.dur*target.val-durations[pair[2]]*target.val)/
      (durations[pair[1]]-durations[pair[2]])
    v2 <- target.val-v1
    res[[i]] <- list(bonds=pair, values=c(v1,v2))
  }
  return(res)
}
