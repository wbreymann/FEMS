#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 03.11.2016
# IDP - Institute for Data Analysis and Process Design
# author(s): Wolfgang Breymann (wolfgang.breymann@zhaw.ch)
#*******************************************************************************


shortNames = c("ID", "Date", "Value", "Type", "Level" , "Curr", "Time" ,  "Nominal", "IR", "Accrued")
names(shortNames) = c("ContractID", "Date", "Value", "Type", "Level","Currency", "Time" , "NominalValue", 
                      "NominalRate", "NominalAccrued")
.defaults = list("shortNames" = shortNames)
.defaults$shortNames

