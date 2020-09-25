#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************

##############################################################
#' \code{priceVsYieldTable}
#'
#' Function which calculates the yield to maturities (YTMs) for a passed
#' contract type and a vector of prices.
#' 
#' @param x a contract type, for which to calculate the YTMs. This can also be 
#'          a timeSeries or EventSeries object.
#' 
#' @param prices a numeric vector, indicating the price used for calculating the YTMs.
#'  
#' @param isPercentage a logical, indicating if the 'price' is passed as percentage or not.
#'                     (default is TRUE). 
#'                     
#' @param from a character indicating the date as for which the YTM is calculated.
#' 
#' @return a data.frame, representing the prices of the contract and the 
#'         yield to maturities (YTM). 
#' 
#' @usage priceVsYieldTable(x, prices, isPercentage, from)
#' 
#' @examples
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            coupon = 0.02, couponFreq = "1 years")
#' ytm_tbl <- priceVsYieldTable(b, prices = c(80, 90, 100))
#' 
#' @include yieldToMaturity.R convexity.R duration.R
#' @export 
priceVsYieldTable <- function(x, prices=NULL, isPercentage=TRUE, from=NULL) {
  if(is.null(prices)) {
    prices <- seq(from=50,to=500,length.out=10)
  }
  res <- data.frame(Price=prices,YTM=numeric(length(prices)))
  for(i in 1:length(prices)) {
    res[i,2] <- yield(x,prices[i],isPercentage, from)
  }
  return(res)
}


##############################################################
#' \code{priceVsYieldPlot}
#'
#' Function which calculates the yield to maturities (YTMs) for a passed
#' contract type and a vector of prices.
#' 
#' @param x a contract type, for which to calculate the YTMs.
#' 
#' @param prices a numeric vector, indicating the price used for calculating the YTMs.
#'  
#' @param isPercentage a logical, indicating if the 'price' is passed as percentage or not.
#'                     (default is TRUE). 
#'                     
#' @param from a character indicating the date as for which the YTM is calculated.
#' 
#' @param drawDuration a logical indicating if the duration should also be plotted.
#' 
#' @param drawConvexity a logical indicating if the convexity should also be plotted..
#' 
#' @return NULL
#' 
#' @usage priceVsYieldPlot(x, prices, isPercentage, from, drawDuration, drawConvexity)
#' 
#' @examples
#' b <- bond("2013-12-31", maturity = "5 years", nominal = 50000, 
#'            coupon = 0.02, couponFreq = "1 years")
#' priceVsYieldPlot(b, prices = c(80, 90, 100))
#' priceVsYieldPlot(b, prices = c(80, 90, 100), drawDuration=TRUE, drawConvexity=TRUE)
#' 
#' @include yieldToMaturity.R convexity.R duration.R
#' @export 
priceVsYieldPlot <- function(x, prices=NULL, isPercentage=TRUE, from=NULL, 
                             drawDuration=FALSE, drawConvexity=FALSE) {
  if(!is.list(x)) {
    x <- list(x)
  }
  if(is.null(prices)) {
    prices <- seq(from=50,to=500,length.out=10)
  }
  res <- lapply(x, FUN=priceVsYieldTable, prices=prices,isPercentage=isPercentage,from=from)
  xlim <- c(min(as.numeric(lapply(res, FUN=function(dat) {min(dat$YTM)}))), 
         max(as.numeric(lapply(res, FUN=function(dat) {max(dat$YTM)}))))
  ylim <- c(min(as.numeric(lapply(res, FUN=function(dat) {min(dat$Price)}))), 
         max(as.numeric(lapply(res, FUN=function(dat) {max(dat$Price)}))))
  
  plot(x=res[[1]][order(res[[1]]$Price,decreasing=TRUE),"YTM"], y=sort(res[[1]]$Price,decreasing=TRUE),
       xlab="YTM", ylab="Price", main="Price vs. YTM Plot", type="n",
       xlim=xlim, ylim=ylim)
  draw=function(dat) {
    points(x=dat[order(dat$Price,decreasing=TRUE),"YTM"], y=sort(dat$Price,decreasing=TRUE))
    lines(x=dat[order(dat$Price,decreasing=TRUE),"YTM"], y=sort(dat$Price,decreasing=TRUE))
  }
  lapply(res, draw)
  if(drawDuration) {
    p.mean <- mean(prices)
    for(i in 1:length(x)) {
      ytm.mean <- yield(x[[i]], price=p.mean, isPercentage=isPercentage, from=from)
      dur <- duration(x[[i]], type="mod", yield=ytm.mean, isPercentage=TRUE)
      lines(x=c(xlim[1], ytm.mean-1/(dur*p.mean)*(ylim[1]-p.mean)), 
            y=c(p.mean-dur*p.mean*(xlim[1]-ytm.mean), ylim[1]), col="red", lwd=2)
    }
  }
  if(drawConvexity) {
    p.mean <- mean(prices)
    for(i in 1:length(x)) {
      ytm.mean <- yield(x[[i]], price=p.mean, isPercentage=isPercentage, from=from)
      yields <- res[[i]]$YTM
      dur <- duration(x[[i]], type="mod", yield=ytm.mean, isPercentage=FALSE)
      cvx <- convexity(x[[i]], yield=ytm.mean, isPercentage=FALSE)
      prices.hat <- p.mean-dur*p.mean*(yields-ytm.mean)+p.mean*cvx*(yields-ytm.mean)^2/2
      lines(x=yields, y=prices.hat, col="blue", lwd=2)
    }
  }
  return(NULL)
}