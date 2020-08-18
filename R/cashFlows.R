#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
cashFlows <- function(x, from=NULL, to=NULL, riskfactors=NULL, variableRate=data.frame()) {
  
# x: FEMS contract or portfolio  
  if(class(x)=="Portfolio") {
    cts <- FEMS:::get(x, "contracts")
    cfs <- cashFlows(cts[[1]], from)
    for(i in 2:length(cts)) cfs=rbind(cfs, cashFlows(cts[[i]], from))
    colnames(cfs) <- c("Value", "Time")
    return(cfs)
  }
  
  if(FEMS:::get(x, "CycleOfRateReset")!="NULL") { # && ncol(variableRate)==0) {
    stop("'x' is a variable rate instrument, hence, please provide 'variableRate' information (a trajectory of the variable rate process)!")
  }
  if(is.null(from)) {
    from <- as.character(FEMS:::get(x, "InitialExchangeDate"))
  }
  if(is.null(to)) {
    to <- as.character(FEMS:::get(x, "MaturityDate"))
  }

  if(is.null(riskfactors)) {
    riskfactors <- FEMS:::get(x, "RiskFactorConnector")$RiskFactorConnector
  }
  if (is(riskfactors,"YieldCurve")) {
    riskfactors <- RFConn(riskfactors)
  } else if (!is.null(riskfactors) & !is(riskfactors,"RiskFactorConnector")) {
    stop("Argument 'riskfactors' must either be of type 'YieldCurve' or 'RiskfactorConnector' !!!")
  }

  if (is.null(riskfactors)) {
    eS <- events(x, from)
  } else {
    eS <- events(x, from, riskfactors)
  }
  cf <- as.data.frame(eS)[,c("Date","Value","Type","Time")]
  cf <- cf[cf$Date>=from,]
  cf <- cf[cf$Date<=to,]
  cf[cf$Type%in%c("RR","RRY","SC","PRY"),"Value"] <- 0
  cf.ts <- timeSeries(cf[,c("Value","Time")], charvec=substring(cf$Date,1,10))
  cf.ts$Time <- as.numeric(cf.ts$Time)
  cf.ts$Value <- as.numeric(cf.ts$Value)
  return(cf.ts)
}