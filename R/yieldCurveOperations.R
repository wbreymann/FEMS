#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
# author(s): Nils Andri Bundi (bund@zhaw.ch)
#*******************************************************************************

shift <- function(x, by, isPercentage=TRUE) {
  if(isPercentage) {
    by <- by/100
  }
  # get base rates of yield curve
  rates <- FEMS:::get(x, "Rates")
  # apply parallel shift
  rates <- rates+by
  # set to yield curve
  set(x, list(Rates=list(Rates=rates)))
  # no need to return object as values are passed "by ref"
}