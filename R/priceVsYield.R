#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
priceVsYieldTable <- function(x, prices=NULL, isPercentage=TRUE, per=NULL) {
  if(is.null(prices)) {
    prices <- seq(from=50,to=500,length.out=10)
  }
  res <- data.frame(Price=prices,YTM=numeric(length(prices)))
  for(i in 1:length(prices)) {
    res[i,2] <- yield(x,prices[i],isPercentage, per)
  }
  return(res)
}

priceVsYieldPlot <- function(x, prices=NULL, isPercentage=TRUE, per=NULL, 
                             drawDuration=FALSE, drawConvexity=FALSE) {
  if(!is.list(x)) {
    x <- list(x)
  }
  if(is.null(prices)) {
    prices <- seq(from=50,to=500,length.out=10)
  }
  res <- lapply(x, FUN=priceVsYieldTable, prices=prices,isPercentage=isPercentage,per=per)
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
      ytm.mean <- yield(x[[i]], price=p.mean, isPercentage=isPercentage, per=per)
      dur <- duration(x[[i]], type="mod", yield=ytm.mean, isPercentage=TRUE)
      lines(x=c(xlim[1], ytm.mean-1/(dur*p.mean)*(ylim[1]-p.mean)), 
            y=c(p.mean-dur*p.mean*(xlim[1]-ytm.mean), ylim[1]), col="red", lwd=2)
    }
  }
  if(drawConvexity) {
    p.mean <- mean(prices)
    for(i in 1:length(x)) {
      ytm.mean <- yield(x[[i]], price=p.mean, isPercentage=isPercentage, per=per)
      yields <- res[[i]]$YTM
      dur <- duration(x[[i]], type="mod", yield=ytm.mean, isPercentage=FALSE)
      cvx <- convexity(x[[i]], yield=ytm.mean, isPercentage=FALSE)
      prices.hat <- p.mean-dur*p.mean*(yields-ytm.mean)+p.mean*cvx*(yields-ytm.mean)^2/2
      lines(x=yields, y=prices.hat, col="blue", lwd=2)
    }
  }
  return(NULL)
}