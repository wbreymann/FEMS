#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 10.11.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************
#' @include duration.R presentValue.R 
#' @export 
immunize <- function(x, target, yield, isPercentage=TRUE, per=NULL) {
  browser()
  target.val <- presentValue(target, yield=yield, isPercentage=isPercentage, per=per)
  target.dur <- duration(target, yield=yield, isPercentage=isPercentage, per=per)
  
  cts <- FEMS:::get(x, "contracts")
  
  durations <- numeric(length(cts))
  for(i in 1:length(cts)) {
    durations[i] <- duration(cts[[i]], type="mac", yield=yield, yieldCurve=NULL, 
                          price=NULL, isPercentage=isPercentage, per=per)
  }
  
  if(max(range(durations))<target.dur || min(range(durations))>target.dur) {
    stop("single bond durations do not support immunization w.r.t. the target duration!")
  }
  
  price <- numeric(length(cts))
  for(i in 1:length(cts)) {
    price[i] <- presentValue(cts[[i]], yield, yieldCurve=NULL, per, isPercentage)
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