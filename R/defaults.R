#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 03.11.2016
# IDP - Institute for Data Analysis and Process Design
# author(s): Wolfgang Breymann (wolfgang.breymann@zhaw.ch)
#*******************************************************************************


shortNames = c("CID", "Date", "Value", "CType", "Level" , "Curr", "Time" ,  "Nominal", "IR", "Accrued",
               "Type","Role","IED","MD","Notional","IR")
names(shortNames) = c("ContractID", "Date", "Value", "Type", "Level","Currency", "Time" , "NominalValue", 
                      "NominalRate", "NominalAccrued","ContractType", "ContractRole", "InitialExchangeDate", 
                      "MaturityDate", "NotionalPrincipal", "NominalInterestRate")
.defaults = list("shortNames" = shortNames)
.defaults$shortNames

.getShortNames <- function(x) {
  sn <- x
  idx <- is.element(x, names(shortNames))
  sn[idx] <- shortNames[sn[idx]]
  #   sN <- shortNames[x]
  # } else {
  #   sN <- x
  # }
  sn
}

