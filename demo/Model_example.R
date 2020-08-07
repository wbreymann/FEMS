# Example for model

rm(list=ls())
devtools::load_all()
library(data.tree)

myModel = Node$new("Simple Model")
myModel$AddChild("Active")
myModel$AddChild("Passive")
myModel$AddChild("Operations")
myModel$Active$AddChild("CurrentAccount")

myModel
