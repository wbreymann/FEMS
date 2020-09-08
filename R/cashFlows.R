#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include RiskFactorConnector.R Events.R
#' @export 
cashFlows <- function(x, from=NULL, to=NULL, riskfactors=NULL, variableRate=data.frame()) {

# x: FEMS contract or portfolio  
  if (class(x)=="Portfolio") {
    cts <- FEMS:::get(x, "contracts")
    cfs <- cashFlows(cts[[1]], from)
    for(i in 2:length(cts)) cfs=rbind(cfs, cashFlows(cts[[i]], from))
    colnames(cfs) <- c("Value", "Time")
    return(cfs)
  } else if (any(is(x) %in% c("Operations", "CurrentAccount"))) {
    
    if(is.null(riskfactors)) {
      riskfactors <- FEMS:::get(x, "RiskFactorConnector")$RiskFactorConnector
    }
    if (is(riskfactors,"YieldCurve")) {
      riskfactors <- RFConn(riskfactors)
    } else if (!is.null(riskfactors) & !is(riskfactors,"RiskFactorConnector")) {
      stop("Argument 'riskfactors' must either be of type 'YieldCurve' or 'RiskfactorConnector' !!!")
    }
    
    if(is.null(from)) {
      from <- as.character(FEMS:::get(x, "ContractDealDate"))
      if(is.null(from)){
        stop("Argument 'from' has to be provided !!!")
      }
    }
    if(is.null(to)) {
      if (class(x)=="Operations") {
        eS <- events(x, from, riskfactors)
        to <- eS$evs$Date[length(eS$evs$Date)]
      } else {
        # use 5 years after ContractDealDate
        print("No end date specified. Default of 5 years from start date is used !")
        to <- as.character(ymd(from) %m+% years(5))
      }
    }
    
  } else {
  
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
  }
  
  if (is.null(riskfactors)) {
    if ((class(x)=="CurrentAccount")){
      eS <- events(x, from, end_date = to)
    } else {
      eS <- events(x, from)
    }
  } else {
    if ((class(x)=="CurrentAccount")){
      eS <- events(x, from, riskfactors, end_date = to)
    } else {
      eS <- events(x, from, riskfactors)
    }
  }
  
  cf <- as.data.frame(eS)[,c("Date","Value","Type","Time")]
  cf <- cf[cf$Date>=from,]
  cf <- cf[cf$Date<=to,]
  cf[cf$Type%in%c("RR","RRY","SC","PRY"),"Value"] <- 0
  cf <- cf[!(cf$Type%in%c("IPCI","DPR")),]
  cf <- cf[!((cf$Date==from) & (cf$Value==0)),]
  cf.ts <- timeSeries(cf[,c("Value","Time")], charvec=substring(cf$Date,1,10))
  cf.ts$Time <- as.numeric(cf.ts$Time)
  cf.ts$Value <- as.numeric(cf.ts$Value)
  ts <- aggregate(cf.ts, time(cf.ts), "sum")
  ts$Time <- cf.ts[row.names(ts),]$Time
  return(ts)
}