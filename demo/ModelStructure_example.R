# Example for model

rm(list=ls())
devtools::load_all()
# library(data.tree)

myModel = ModelStructure("Minimal Model")
myModel
myModel$Active$Treasury$contracts

addActive("Mortgages", myModel)
myModel

class(myModel$Active)

addContracts(list(CurrentAccount()), myModel$Active) # error because account is not a leaf
addContracts(list(CurrentAccount()), myModel$Active$Mortgages)

myModel$Active$Mortgages$contracts

is.null(FindNode(myModel, "Mortgages"))

addContracts(list(CurrentAccount()), FindNode(myModel, "Mortgages"))
length(myModel$Active$Mortgages$contracts)

