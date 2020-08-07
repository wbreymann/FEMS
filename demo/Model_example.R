# Example for model

rm(list=ls())
devtools::load_all()
# library(data.tree)

myModel = Model("Minimal Model")
myModel
myModel$Active$Treasury$contracts

addActive("Mortgages", myModel)
myModel

addContracts(CurrentAccount(), myModel$Active) # error because account is not a leaf
addContracts(CurrentAccount(), myModel$Active$Mortgages)

myModel$Active$Mortgages$contracts


myModel$Active$isLeaf
myModel$Active$Mortgages$contracts
is.null(myModel$Active$Mortgages[["contracts"]])

CurrentAccount()
