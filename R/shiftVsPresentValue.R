#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include cashFlows.R YieldCurve.R DynamicYieldCurve.R
#' @export 
shiftVsValueTable <- function(x, yieldCurve, shift=NULL, isPercentage=TRUE, per=NULL) {
  if(is.null(shift)) {
    shift <- seq(from=1,to=10,length.out=10)
  }
  if(isPercentage) {
    shift <- shift/100
  }
  if(is.null(per)) {
    per <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  baseRates <- FEMS:::get(yieldCurve, "Rates")
  cf <- cashFlows(x, per=per)
  dates <- as.character(time(cf))
  res <- data.frame(Shift=shift,PresentValue=numeric(length(shift)))
  for(i in 1:length(shift)) {
    set(yieldCurve, list(Rates=list(Rates=baseRates+shift[i])))
    df <- discountFactors(yieldCurve, dates,isDateEnd=TRUE)
    res[i,2] <- as.numeric(t(cf$Value)%*%df)
  }
  set(yieldCurve, list(Rates=list(Rates=baseRates)))
  return(res)
}

shiftVsValuePlot <- function(x, yieldCurve, shift=NULL, isPercentage=TRUE, per=NULL) {
  if(!is.list(x)) {
    x <- list(x)
  }
  res <- lapply(x, FUN=shiftVsValueTable, yieldCurve=yieldCurve, shift=shift,isPercentage=isPercentage,per=per)
  xlim <- c(min(as.numeric(lapply(res, FUN=function(dat) {min(dat$Shift)}))), 
         max(as.numeric(lapply(res, FUN=function(dat) {max(dat$Shift)}))))
  ylim <- c(min(as.numeric(lapply(res, FUN=function(dat) {min(dat$PresentValue)}))), 
         max(as.numeric(lapply(res, FUN=function(dat) {max(dat$PresentValue)}))))
  
  plot(x=sort(res[[1]]$Shift,decreasing=FALSE), y=res[[1]][order(res[[1]]$Shift,decreasing=FALSE),"PresentValue"],
       xlab="shift", ylab="PresentValue", main="Present Value vs. Shift", type="n",
       xlim=xlim, ylim=ylim)
  draw <- function(dat) {
    points(x=sort(dat$Shift,decreasing=FALSE), y=dat[order(dat$Shift,decreasing=FALSE),"PresentValue"])
    lines(x=sort(dat$Shift,decreasing=FALSE), y=dat[order(dat$Shift,decreasing=FALSE),"PresentValue"])
  }
  lapply(res, draw)
  return(NULL)
}