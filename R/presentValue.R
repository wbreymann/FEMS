#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
presentValue <- function(x, yield=NULL, yieldCurve=NULL, per=NULL, isPercentage=TRUE) {
  
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
      pv + presentValue(cts[[i]], yield[i], yieldCurve, per, isPercentage)
    return(pv)
  }
  
  # date as per which 'yieldCurve' is valid 
  # (and hence, as per which we calculate present value)
  if(is.null(per)) {
    per <- as.character(FEMS:::get(x,"InitialExchangeDate"))
  }
  
  # compute cash flows of instrument
  cf <- cashFlows(x, per=per)
  
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