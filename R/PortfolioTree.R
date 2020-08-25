#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A hierarchical tree structure for the organization of a portfolio
#' 
#' This Reference Class represents a hierarchical tree structure
#' that allows organizing a portfolio of contracts into sub-portfolios
#' as is the case e.g. for an organization's balance sheet.
#' 
#' A tree can then be passed to methods \link{value}, \link{liquidity}, \link{income} 
#' by specifying the optional argument \code{tree=}. If a tree is specified, these
#' methods will then compute and return the respective quantities on the various
#' aggregate levels in the tree's hierarchy rather than the granular contract level.
#' 
#' @field branches a list of intermediate sub-portfolios each having a name and
#' a vector indicating the names of its sub-portfolios.
#' 
#' @field leafs a list of terminal sub-portfolios to which the contracts 
#' of the \code{Portfolio} are assigned. Terminal sub-portfolio here means that
#' a \code{leaf} can itself not have sub-portfolios but only contracts assigned
#' to it. Hence, a leaf has a name and a vector indicating the \code{ContractID}s
#' of its contracts.
#' 
#' @seealso \link{Portfolio}, \link{ContractType}
#' 
#' @examples
#' tree=Tree(list(
#'                branches=list(B0=c("B1","B2"),
#'                              B1="L1",
#'                              B2=c("L2","L3")),
#'                leafs=list(L1=c("106","107"),
#'                           L2=c("108","109"),
#'                           L3=c("110","111"))
#'                 ))
#' tree
#' 
## @include
#' @export
#' @rdname tree-class
setRefClass("PortfolioTree",
            fields = list(
              leafs = "list",
              branches = "list"))

##############################################################
#' Create a new \code{PortfolioTree}
#'
#' This method creates a new \code{PortfolioTree}-object.
#' 
#' @param ... (optional) Comma-separated elements (or a list of elements) specifying field values to be attached to the new object
#'
#' @return A new \code{PortfolioTree}-object
#' 
#' @seealso 
#' 
#' @examples
#' tree=Tree(list(
#'                branches=list(B0=c("B1","B2"),
#'                              B1="L1",
#'                              B2=c("L2","L3")),
#'                leafs=list(L1=c("106","107"),
#'                           L2=c("108","109"),
#'                           L3=c("110","111"))
#'                 ))
#' tree
#' 
## @include
#' @export
#' @rdname tree-methods
setGeneric(name = "Tree",
           def = function(...){
             standardGeneric("Tree")
           })

## @include
#' @export
#' @rdname tree-methods
setMethod(f = "Tree",
          definition = function(...){
            out = new("PortfolioTree")
            pars=list(...)
            if(length(pars)>0) {
              FEMS:::set(out,what=pars[[1]])
            }
            return(out)
          })

##############################################################
#' Get the value of a \code{PortfolioTree}-field
#'
#' A convenience-wrapper to access values of the fields of a
#' \code{PortfolioTree}.
#' 
#' @param object A \code{PortfolioTree}-object
#'        
#' @param what A character (-vector) specifying the names of fields to return values for
#'
#' @return A list of values of the specified fields
#' 
#' @seealso \link{set}
#' 
#' @examples
#' tree=Tree()
#' get(tree,what="leafs")
#' set(tree, what=list(leafs=list(L1=c(1001,1002),L2=c(1003,1004))))
#' get(tree,what="leafs")
#'
## @include
#' @export
#' @docType methods
#' @rdname get-methods
setMethod(f = "get",
          signature = c("PortfolioTree","character"),
          definition = function(object,what){
            out=list()
            for(field in what) {
              out[[field]]=object[[field]]
            }
            if(length(out)==1) out=out[[1]]
            return(out)
          })

##############################################################
#' Change the value of a \code{PortfolioTree}-field
#'
#' A convenience-wrapper to change values of the fields of a
#' \code{PortfolioTree}.
#' 
#' @param object A \code{PortfolioTree}-object
#'        
#' @param what A list with names and values of fields to change
#'
#' @return
#' 
#' @seealso \link{get}
#' 
#' @examples
#' tree=Tree()
#' set(tree, what=list(leafs=list(L1=c(1001,1002),L2=c(1003,1004))))
#'
## @include
#' @export
#' @docType methods
#' @rdname set-methods
setMethod(f = "set",
          signature = c("PortfolioTree","list"),
          definition = function(object,what){
            
            for(field in names(what)) {
              object[[field]]=what[[field]]
            }
            
          })

## @include
#' @export
# @docType methods
# @rdname add-methods
setMethod(f = "show",signature = c("PortfolioTree"),
          definition = function(object){
            print("branches:")
            print(object$branches)
            print("leafs:")
            print(object$leafs)
          })

# util function for tree aggregation
aggregate.leafs=function(leafs,branches,col.names) {
  out=leafs
  if(length(branches)>0) {
    treemap=unlist(branches)
    names(treemap)=unlist(sapply(names(branches),FUN=function(x) paste(x,1:length(branches[[x]]),sep=".")))
    child=leafs
    idx=treemap%in%names(child)
    namesmap=unlist(lapply(strsplit(names(treemap),".",fixed=TRUE),function(x)x[1]))
    names(namesmap)=treemap
    names(out)=paste(namesmap[names(out)],names(out),sep=":")
    while(sum(idx)>0) {
      parent=as.list(unique(unlist(lapply(strsplit(names(treemap)[idx],".",fixed=TRUE),FUN=function(x)x[1]))))
      temp=lapply(parent,FUN=function(x) {
        apply(as.data.frame(child[treemap[grep(x,names(treemap))]]),1,sum)
      })
      names(temp)=unlist(parent)
      out=c(temp,out)
      treemap=treemap[!idx]
      child=temp
      idx=treemap%in%names(child)
      if(sum(idx)>0) {
        namesmap=unlist(lapply(strsplit(names(treemap),".",fixed=TRUE),function(x)x[1]))
        names(namesmap)=treemap
        names(out)=paste(namesmap[unlist(lapply(strsplit(names(out),":",fixed=TRUE),function(x)x[1]))],names(out),sep=":")
      }
    }
  }
  out=FEMS:::format.tree.results(out,col.names)
  return(out)
}

# util function for tree formatting
format.tree.results=function(x, col.names) {
  out=t(as.data.frame(x))
  out=as.data.frame(out[sort(rownames(out)),])
  colnames(out)=as.character(col.names[-1])
  return(out)
}