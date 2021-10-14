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
#' @export
#' @rdname modelstructure
#' @export
## Das Problem hier ist, dass man dann nicht mehr auf die Methoden von "Node"
## zugreifen kann, weil die offenbar private sind.
setGeneric(name = "ModelStructure",
           def = function(name, type, ...){
             standardGeneric("ModelStructure")
           })

#' @export
setMethod(f = "ModelStructure", signature = c("character", "character"),
          definition = function(name, type, curAcc=CurrentAccount()){
            object <- Node$new(name)
            if (type=="institution") {
              object$AddChild("Active")
              object$AddChild("Passive")
              object$AddChild("Operations")
              object$Active$AddChild("Treasury")
              # Create a contract of type "CurrentAccount" in account "Treasury"
              ll = list()
              ll[[curAcc$ContractID]] <- curAcc
              object$Active$Treasury$contracts <- ll
            } else if (!type=="portfolio") {
              stop("type must be 'institution' or 'portfolio'")
            }
            return(object)
          })

#' @export
setMethod(f = "ModelStructure", signature = c("character", "missing"),
          definition = function(name, type, curAcc=CurrentAccount()){
            return(ModelStructure(name, type="institution", curAcc = curAcc))
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

#' @export
setGeneric(name = "add.model",
           def = function(object, added.object, ...){
             standardGeneric("add.model")
           })

#' @export
setMethod(f = "add.model", signature = c("Node", "Node"),
          definition = function(object, added.object, ...){
            if (!identical(object$Get("levelName"), added.object$Get("levelName"))) {
              stop("Structure of both nodes must be identical!")
            }
            out <- make.copy(object)
            out$Do(function(node) {
                      nm <- names(node$Get("levelName"))
                      ctrs <- FindNode(added.object, nm)$contracts
                      if (!is.null(ctrs)) {
                        addContracts(ctrs, node)
                      }
                      }, filterFun = isLeaf)
            clearAnalytics(out, "eventList")
            clearAnalytics(out, "value")
            clearAnalytics(out, "income")
            clearAnalytics(out, "liquidity")
            return(out)
          })

#' @export
setGeneric(name = "make.copy",
           def = function(object, ...){
             standardGeneric("make.copy")
           })

#' @export
setMethod(f = "make.copy", signature = c("Node"),
          definition = function(object, empty = FALSE, ...){
            object.copy <- Clone(object, pruneFun = NULL, attributes = FALSE)
            if (empty) {
              clearAll(object.copy)
            }
            return(object.copy)
          })

#' @export
setMethod(f = "get", signature = c("Node", "character"),
          definition = function(object, what, ...){
            if ( tolower(what[1]) == "contracts" ) {
              if(!is.list(object$Get("contracts"))) {
                out <- list()
              } else {
                out <- Filter(Negate(anyNA), object$Get("contracts"))
              }
            }
            return(out)
          })


####---------------------------------------------------------------
## events methods

#' @include Events.R
#' @rdname ev-methods
#' @export
setMethod(f = "events", signature = c("Node", "character", "RiskFactorConnector"),
          definition = function(object, ad, model, end_date){
            # The function 'events.modelstructure' is applied to all nodes.
            object$Do(fun=events.modelstructure, ad=ad, model=model, end_date=end_date)

            # fill Cash Collector, if it exists
            if (!is.null(object$Assets$Current)) {
                # transfer this to cash collector
                transfer.to.collector(object, ad, get.YieldCurve(model), end_date)
            }
          })


#' @include CurrentAccount.R
###' @export
events.modelstructure = function(node, ..., filterFun=isLeaf) {
  
  node$eventList <- NULL # cleanup old eventList
  pars = list(...)
  ctrs = node$contracts
  # print(paste("Klasse", class(ctrs[[1]])))
  
  res = sapply( # applies 'events' method to all contracts.
    X=ctrs,
    FUN = function(x, pars) {
      pars = c(object=x, pars)
      if ( class(x)!="CurrentAccount") { pars[["end_date"]] <- NULL }
      if ( class(x)=="CurrentAccount" && 
           names(node$contracts)=="collector") { 
        # do nothing in this case...
      } else {
      # print(paste("Parameter: Anzahl", length(pars)))
      # print(class(x))
      # print(pars)
        evs = do.call("events", pars) 
        if (!is.null(evs) ) {
          if (is.null(node$eventList)) {
            node$eventList <- eventList()
          }
          node$eventList[[as.character(get(x,"ContractID"))]] <- evs
        }
      }
    }, pars)
}

transfer.to.collector <- function(object, ad, model, end_date) {
  
  evList <- object$Get("eventList")
  evList.filtered <- data.frame()
  for (i in 1:length(evList)) {
    if (!is.na(evList[[i]][1])) {
      tempList <- as.data.frame(evList[[i]])
      tempList <- tempList[!(tempList$Value %in%c("IPCI","DPR","PRF","RR","RRY","SC","PRY")),
                           c("Date","Value")]
      evList.filtered <- rbind(evList.filtered, tempList)
    }
  }

  # aggregate this and add to Cash Collector...
  evAgg <-aggregate(x = list(Value = evList.filtered$Value),
                by = list(Date = evList.filtered$Date),
                FUN = sum)
  internal.tfs <- timeSeries(data = evAgg$Value,
                             charvec = evAgg$Date,
                             units = "Values")
  # t0 <- as.character(rownames(internal.tfs[1,]))
  internal.tfs <- internal.tfs[internal.tfs$Values!=0,]
  set(object$Assets$Current$contracts$collector, 
                list(InternalTransfers = internal.tfs,
                     CycleAnchorDateOfInterestPayment = ad,
                     CycleAnchorDateOfRateReset = ad,
                     ContractDealDate = ad,
                     MarketObjectCodeRateReset = model$label))
  
  # calculate events of cash collector
  evs <- events(object$Assets$Current$contracts$collector, 
                ad, model, end_date=as.character(end_date))

  # add this to the eventList
  nm <- as.character(get(object$Assets$Current$contracts$collector,"ContractID"))
  object$Assets$Current$eventList[[nm]] <- evs
}

get.YieldCurve <- function(rfconn) {
  tst <- unlist(lapply(rfconn$riskfactors, function(x) 
                      {class(x) %in% c("DynamicYieldCurve","YieldCurve")}))
  if (sum(tst) > 1) {
    stop("More than one YieldCurve object supplied. Currently not supported!")
  }
  yc <- rfconn$riskfactors[tst][[1]]
  return(yc)
}

####---------------------------------------------------------------
## liquidity methods

#' @include Liquidity.R
#' @rdname liq-methods
#' @export
setMethod(f = "liquidity", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, scale=1, digits=2){
            if (missing(type)) {
              type <- "marginal"
            }
            # Compute liquidity for whole tree
            clearAnalytics(object, "liquidity")
            object$Do(fun=fAnalytics, "liquidity", by=by, type=type, 
                      filterFun=isLeaf)
            aggregateAnalytics(object, "liquidity")
            res = data.frame(
              t(object$Get("liquidity", format = function(x) as.numeric(ff(x,0))) ),
              check.names=FALSE, fix.empty.names=FALSE)
            rownames(res) = capture.output(print(object))[-1]
            colnames(res) <- by@bucketLabs
            return(round(res/scale,digits))
          })

####---------------------------------------------------------------
## value methods

#' @include Value.R
#' @rdname val-methods
#' @export
setMethod(f = "value", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, method, scale=1, digits=2) {

            if (missing(method)) {
              method <- DcEngine()
            }
            if (missing(type)) {
              type <- "nominal"
            }
            # Compute value for whole tree
            clearAnalytics(object, "value")
            object$Do(fun=fAnalytics, "value", by=as.character(by), type=type, 
                      method=method, filterFun=isLeaf)
            aggregateAnalytics(object, "value")
            object$Equity$value <- -object$value
            object$value <- rep(0, length(object$value))
            object2 <- Clone(object)
            if ( type == "nominal" && is.element("PandL", names(object2$children)) )
              object2$RemoveChild("PandL")
            
            res <- data.frame(
              t(object2$Get("value", format = function(x) as.numeric(ff(x,0)))  ),
              check.names=FALSE, fix.empty.names=FALSE)
            rownames(res) <- capture.output(print(object2))[-1]
            colnames(res) <- by@breakLabs
            return(round(res/scale,digits))
          })


####---------------------------------------------------------------
## income methods

#' @include Income.R
#' @rdname inc-methods
#' @export
setMethod(f = "income", signature = c("Node", "timeBuckets", "ANY"),
          definition = function(object, by, type, revaluation.gains, 
                                method, scale=1, digits=2){

            # Compute income for whole tree
            if (missing(method)) {
              method <- DcEngine()
            }
            if (missing(type)) {
              type <- "marginal"
            }
            if (missing(revaluation.gains)) {
              revaluation.gains <- FALSE
            }
            clearAnalytics(object, "income")
            object$Do(fun=fAnalytics, "income", by=by, type=type, method=method, 
                      revaluation.gains=revaluation.gains, filterFun=isLeaf)
            aggregateAnalytics(object, "income")
            res <- data.frame(
              t(object$Get("income", format = function(x) as.numeric(ff(x,0))) ),
              check.names=FALSE, fix.empty.names=FALSE)
            rownames(res) <- capture.output(print(object))[-1]
            colnames(res) <- by@bucketLabs
            return(round(res/scale,digits))
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
  if ( is.null(node$eventList) || length(node$eventList)==0 ) {
    node[[ pars[[1]] ]] <- rep(0, length(pars[["by"]]))
    if ( is.null(names(pars[["by"]])) ) {
      names(node[[pars[[1]] ]]) = as.character(pars[["by"]])
    } else {
      names(node[[pars[[1]] ]]) = names(pars[["by"]])
    }
  } else {
    ctrs = node$contracts
    res = sapply(
      X=ctrs,
      FUN = function(x, pars) {
        pars = list(...)
        fnam = pars[[1]] # the name of the analytics [liquidity|income|value]
        Id = tryCatch({
          get(x,"ContractID")
        }, error = function(e) {
          x
        })
        object = node$eventList[[as.character(Id)]] # the eventSeries of the contract
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
}

# This function aggregates the results computed by fAnaytics
aggregateAnalytics = function(node, analytics) {
  if (!isLeaf(node)) {
    res = sapply(
      node$children,
      FUN=function(child, analytics) {
        x = analytics
        if (!is.null(child[[x]])) {
          child[[x]]
        } else if (!isLeaf(child)) {
          aggregateAnalytics(child, analytics=x)
        }
      }, analytics=analytics, simplify=TRUE)
    if ( !is.null(dim(res)) ) res = rowSums(res)
    node[[analytics]] = res
  }
}

# Clears previously computed the analytics "analytics" from the tree "node"
clearAnalytics = function(node, analytics) {
  nodes = Traverse(node, traversal="pre-order")
  for (n in nodes) {
    n[[analytics]] = NULL
  }
}

# clears all attributes of a node
clearAll = function(node) {
  nodes = Traverse(node, traversal="pre-order")
  for (n in nodes) {
    att <- n$attributes
    for (a in att) {
      n[[a]] = NULL
    }
  }
}

# Clears previously computed the analytics "analytics" from the tree "node"
clearAnalytics = function(node, analytics) {
  node[[analytics]] = NULL
  nodes = Traverse(node, traversal="pre-order")
  for (n in nodes) {
    n[[analytics]] = NULL
  }
}

#' Clears previously computed the analytics "analytics" from the tree "node"
#' @export
clearEvents = function(node) {
  clearAnalytics(node, "eventList")
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
