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
           def = function(name, ...){
             standardGeneric("ModelStructure")
           })

#' @export
setMethod(f = "ModelStructure", signature = c("character"),
          definition = function(name, curAcc=CurrentAccount()){
            object <- Node$new(name)
            object$AddChild("Active")
            object$AddChild("Passive")
            object$AddChild("Operations")
            object$Active$AddChild("Treasury")
            # Create a contract of type "CurrentAccount" in account "Treasury"
            object$Active$Treasury$contracts = list(currentAccount=curAcc)
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

####---------------------------------------------------------------
## events methods

#' @include Events.R
#' @rdname ev-methods
#' @export
setMethod(f = "events", signature = c("Node", "character", "RiskFactorConnector"),
          definition = function(object, ad, model, end_date){
            object$Do(fun=events.modelstructure, ad=ad, model=model, end_date=end_date)
          })


#' @include CurrentAccount.R
###' @export
events.modelstructure = function(node, ..., filterFun=isLeaf) {
  node$eventList <- NULL # cleanup old eventList
  pars = list(...)
  ctrs = node$contracts
  res = sapply(
    X=ctrs,
    FUN = function(x, pars) {
      pars = c(object=x, pars)
      if ( class(x)!="CurrentAccount") { pars[["end_date"]] <- NULL }
      evs = do.call("events", pars)
      if (!is.null(evs) ) {
        if (is.null(node$eventList)) {
          node$eventList <- eventList()
        }
        node$eventList[[x$ContractID]] <- evs
      }
    }, pars)
}

####---------------------------------------------------------------
## liquidity methods

#' @include Liquidity.R
#' @rdname liq-methods
#' @export
setMethod(f = "liquidity", signature = c("Node", "timeBuckets", "character"),
          definition = function(object, by, type){
            # Compute liquidity for whole tree
            clearAnalytics(Node, "liquidity")
            object$Do(fun=fAnalytics, "liquidity", by=by, type=type, filterFun=isLeaf)
            aggregateAnalytics(object, "liquidity")
            liq = data.frame (t(object$Get("liquidity", format = function(x) ff(x,0))  ),
                              check.names=FALSE, fix.empty.names=FALSE)
            rownames(liq) = capture.output(print(object))[-1]
            liq
          })

####---------------------------------------------------------------
## value methods

#' @include Value.R
#' @rdname val-methods
#' @export
setMethod(f = "value", signature = c("Node", "timeBuckets", "character"),
          definition = function(object, by, type){
            # Compute value for whole tree
            clearAnalytics(Node, "value")
            object$Do(fun=fAnalytics, "value", by=by, type=type, filterFun=isLeaf)
            aggregateAnalytics(object, "value")
            liq = data.frame (t(object$Get("value", format = function(x) ff(x,0))  ),
                              check.names=FALSE, fix.empty.names=FALSE)
            rownames(liq) = capture.output(print(object))[-1]
            liq
          })

##################################################
#' general function for computing analytics on a data.tree structure of class Node
#'
#' This function computes analytics individually for the leafs of a tree
#' The analytics to be computed must be passed as first argument.
#' This function thus subsumes the function of all three specialized 
#' functions above (which are commented out)
fAnalytics = function(node, ...) {
  pars = list(...)
  # clear analytics
  node[[ pars[[1]] ]] <- NULL
  
  ctrs = node$contracts
  res = sapply(
    X=ctrs,
    FUN = function(x, pars) {
      pars = list(...)
      fnam = pars[[1]] # the name of the analytics [liquidity|income|value]
      object = node$eventList[[x$ContractID]] # the eventSeries of the contract
      pars = pars[c(-1)]
    do.call(fnam, c(object=object, pars))
    })
  if (!is.null(dim(res)) ) {
    res = rowSums(res)
  } else if (length(res) == 0) {
    res <- NULL
  }
  node[[pars[[1]] ]] = res
}

# This function aggregates the results computed by fAnaytics
aggregateAnalytics = function(node, analytics) {
  # print("parents")
  # print(node)
  res = sapply(
    node$children,
    FUN=function(child, analytics) {
      x = analytics
      if (!is.null(child[[x]])) {
        # print("child:")
        # print(child)
        # print("Child non-zero, copied")
        # print(child[[x]])
        child[[x]]
      } else if (!isLeaf(child)) {
        # print("child:")
        # print(child)
        # print("Child zero, aggregate executed")
        aggregateAnalytics(child, analytics=x)
        # print(child[[x]])
      }
    }, analytics=analytics, simplify=TRUE)
  if ( !is.null(dim(res)) ) res = rowSums(res)
  # print("Final")
  node[[analytics]] = res
  # print(node)
  # print(node[[analytics]])
  # return(res)
}

# Clears previously computed the analytics "analytics" from the tree "node"
clearAnalytics = function(node, analytics) {
  nodes = Traverse(node, traversal="pre-order")
  for (n in nodes) {
    n[[analytics]] = NULL
  }
}

# Formatting function.
# Notice that the 'ifelse' command doesn't return the rigth result.
ff = function (x, digits = 3) 
{
  # print (x)
  # sprintf(paste0("%.", digits, "f"), x)
  if (is.null(x) || is.na(x) ) {
    ch = ""
  } else {
    ch = sprintf(paste0("%.", digits, "f"), x)
    names(ch) = names(x)
  }
  return(ch)
}

#-----------------------------------------------------------------------------
# Generate data.tree like analysis structure from old balance sheet structure
# this and the following helper function transforms an object of class Tree
# into a nested list that can be used to created an object of class Node 
# as defined in the data.tree package
Tree.to.nested.list = function(tree) {
  mat = as.matrix(as.data.frame(tree$branches))
  nams = names(tree$branches)
  root = nams[!(nams %in% mat)]
  list.from.matrix(mat, root, tree$leafs)
}

list.from.matrix = function(x, root, leafs) 
  # Recursive function to transform an object of type Tree into 
  # a nested list structure where leafs are simply objects without nodes
{
  l = list()
  if ( !is.null(dim(x)) ) {
    mat = x[,!colnames(x) %in% root]
    for (subroot in x[,root]) {
      if ( subroot %in% colnames(mat)) {
        l[[subroot]] = list.from.matrix(mat, subroot, leafs)
      } else {
        l[[subroot]] = list(ctNames = leafs[[subroot]])
      }
    }
  } else {
    # l[[root]] = x
    # for (acc in x) {
    #   l[[root]][[acc]] = list(ctNames = leafs[[acc]])
    # }
  }
  return(l)
}


##################################################################
# Utility functions for working with data.tree type analysis structure
# util function for tree aggregation
# liqfun = function(node, portfolio, tb) {
#   nams = node$ctNames
#   res = sapply(
#     X=nams,
#     FUN = function(x) {
#       liquidity(portfolio[[x]], tb)
#     })
#   liq = rowSums(res)
#   print(liq)
#   node$liquidity = liq
# }

# liqfun = function(node, ...) {
#   pars = list(...)
#   # print(pars)
#   nams = node$ctNames
#   res = sapply(
#     X=nams,
#     FUN = function(x, pars) {
#       pars = list(...)
#       object = pars[[1]][[x]]
#       pars = pars[-1]
#       do.call("liquidity", unlist(list(object, pars)))
#     })
#   liq = rowSums(res)
#   # print(liq)
#   node$liquidity = liq
# }
# 

# aggregateLiquidity = function(node) {
#   liq = sapply(
#     node$children,
#     function(child) {
#       if (!is.null(child$liquidity)) {
#         child$liquidity
#       } else {
#         aggregateLiquidity(child)
#       }
#     })
#   if ( !is.null(dim(liq)) ) liq = rowSums(liq)
#   node$liquidity = liq
#   # print(liq)
#   return(liq)
# }
