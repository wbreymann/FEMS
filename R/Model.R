#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

library(data.tree)

##############################################################
#' 
#' Class that contains the whole model of an enterprise or institution.
#' 
#' The class is implemented as a data.tree structure. 
#' It contains a hierarchical model structure.
#' The upper level is predefined with the nodes "Active", "Passive" and
#' "Operations".
#' 
#' @include 
#' @export
#' @rdname model


#' @export
setGeneric(name = "Model",
           def = function(...){
             standardGeneric("Model")
           })

#' @export
setMethod(f = "Model",signature = c("character"),
          definition = function(name){
            object <- Node$new(name)
            object$AddChild("Active")
            object$AddChild("Passive")
            object$AddChild("Operations")
            object$Active$AddChild("Treasury")
            # Create a contract of type "CurrentAccount" in account "Treasury"
            object$Active$Treasury$contracts = list(currentAccount=CurrentAccount())
            return(object)
          })


## Add Accounts
## This can be done by hand (easy to implement) or from a flat file
addActive <- function(account, model)
{
  model$Active$AddChild(account)
}

addPassive <- function(account, model)
{
  model$Passive$AddChild(account)
}

addOperations <- function(account, model)
{
  model$Operations$AddChild(account)
}

addContracts <- function(contracts, leaf)
{
  stopifnot(leaf$isLeaf)
  if (is.null(leaf$contracts))
    leaf$contracts = list()
  leaf$contracts = c(leaf$contracts,contracts)
}
