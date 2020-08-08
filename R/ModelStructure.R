#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

#' @import data.tree
setOldClass("Node")

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
#' @rdname modelstructure

#' @export
## Das Problem hier ist, dass man dann nicht mehr auf die Methoden von "Node"
## zugreifen kann, weil die offenbar private sind.
setGeneric(name = "ModelStructure",
           def = function(...){
             standardGeneric("ModelStructure")
           })

#' @export
setMethod(f = "ModelStructure", signature = c("character"),
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

#' @export
setGeneric(name = "addContracts",
           def = function(contracts, leaf, ...){
             standardGeneric("addContracts")
           })

#' @export
setMethod(f = "addContracts", signature = c("list", "Node"),
          definition = function(contracts, leaf){
            stopifnot(leaf$isLeaf)
            if (is.null(leaf$contracts))
              leaf$contracts = list()
            leaf$contracts = c(leaf$contracts,contracts)
          })

