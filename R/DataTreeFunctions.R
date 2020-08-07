#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

############### for AnalysisTree with data.tree package #######################
library(data.tree)

# This and the following helper function transforms an object of class Tree
# into a nested list that can be used to created an object of class Node 
# as defined in the data.tree package
# They are for backwards compatibility and will no longer be needed 
# when the old class Tree is no longer used.
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

##---------------------------------------------------------------------
# This function computes analytics individually for the leafs of a tree
# Arguments:
# node: Object of R6 class Node containing the analysis structure, 
#       e.g.a balance sheet
# ...: Additional arguments.
#      The name of the analytics must be passed as the first argument of ... 
#      in form of a character string (one of ("liquidity"|"income"|"value")). 
#      The next one should be an object of class "Portfolio" are an ACTUS
#      contract type, as required as the first argument by the analysis function
#      The following argument must be named arguments required by the special
#      analytical function
fAnalytics = function(node, ...) {
  pars = list(...)
  # print(pars)
  nams = node$ctNames
  res = sapply(
    X=nams,
    FUN = function(x, pars) {
      pars = list(...)
      fnam = pars[[1]] # the name of the analytics [liquidity|income|value]
      object = pars[[2]][[x]] # the contracts
      # print(x)
      pars = pars[c(-1,-2)]
      do.call(fnam, unlist(list(object, pars)))
    })
# print(res)
# print(dim(res))
  if (!is.null(dim(res)) ) {
    res = rowSums(res)
  }
  # print(liq)
  node[[pars[[1]] ]] = res
}

# This function aggregates the results computed by fAnaytics for the 
# analytics "analytics"
# 
# Arguments:
# node: Object of R6 class Node containing the analysis structure, 
#       e.g.a balance sheet
# analytics: character string indicating the kind of analytics to be computed.
#            Should be one of ("liquidity"|"income"|"value")
aggregateAnalytics = function(node, analytics) {
  if (class(node$children) != "NULL") {
    res = sapply(
      node$children,
      function(child, analytics) {
        x = analytics
        if (!is.null(child[[x]])) {
          child[[x]]
        } else {
          aggregateAnalytics(child, analytics=x)
        }
      }, analytics=analytics)
  } else {
    res = node[[analytics]]
  }
  
  if ( !is.null(dim(res)) ) 
    res = rowSums(res)
  else if ( length(res) > 1 )
    res = sum(res)
  node[[analytics]] = res
  return(res)
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
