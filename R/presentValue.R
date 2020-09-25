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
#' @param x a contract type, for which to calculate the NPV. This can also be 
#'          a timeSeries or EventSeries object.
#' 
#' @param yield a numeric, indicating the percentage yield used to discount.
#' 
#' @param yieldCurve an object of type \code{\link{YieldCurve}} or 
#'                    \code{\link{DynamicYieldcurve}} to calculate discount 
#'                    factors from.
#'
#' @param from a character indicating the date as for which the NPV is calculated.
#'  
#' @param isPercentage a logical, indicating if the 'yield' is passed as percentage or not.
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
#' evs <- events(b, "2013-12-31")
#' npv <- presentValue(evs, yield = 1)
#' ts <- timeSeries(data = c(-50000, 1000, 1000, 1000, 1000, 51000),
#'                  charvec = c("2013-12-31", "2014-12-31", "2015-12-31", 
#'                  "2016-12-31", "2017-12-31", "2018-12-31"),
#'                  units = "Value")
#' npv <- presentValue(ts, yield = 1)
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
  
  # compute cash flows of instrument
  if (class(x)=="timeSeries") {
    cf <- x
    if (!("Time" %in% colnames(cf))) {
      cf$Time <- yearFraction(rownames(ts)[1], rownames(ts))
    }
  } else if (class(x)=="EventSeries") {
    if(is.null(from)) {
      from <- as.character(evs$evs$Date[1])
    }
    evs <- as.data.frame(x)[,c("Date","Value","Type","Time")]
    evs <- evs[evs$Date>=from,]
    #evs[evs$Type%in%c("RR","RRY","SC","PRY"),"Value"] <- 0
    evs <- evs[!(evs$Type%in%c("IPCI","DPR","PRF","RR","RRY","SC","PRY")),]
    evs <- evs[!((evs$Type %in% "AD0") & (evs$Value==0)),]
    if (evs$Type[dim(evs)[1]]!="MD" & evs$Value[dim(evs)[1]]==0){
      evs <- evs[1:dim(evs)[1]-1,]
    }
    evs.ts <- timeSeries(evs[,c("Value","Time")], charvec=substring(evs$Date,1,10))
    evs.ts$Time <- as.numeric(evs.ts$Time)
    evs.ts$Value <- as.numeric(evs.ts$Value)
    cf <- aggregate(evs.ts, time(evs.ts), "sum")
    cf$Time <- evs.ts[row.names(cf),]$Time
  } else {
    if(is.null(from)) {
      from <- as.character(FEMS:::get(x,"InitialExchangeDate"))
    }
    cf <- cashFlows(x, from=from)
  }
  
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