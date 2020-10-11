
setOldClass("list")
# setRefClass("Portfolio2",
#            contains = "list",
#            fields = list(
#              rf_connector = "RiskFactorConnector",
#              ct_events = "data.frame"
#            ))

setClass("Portfolio2",
            contains = "list",
            slots = list(
              rf_connector = "RiskFactorConnector",
              ct_events = "data.frame"
            ))

## @include
#' @export
#' @docType methods
## @rdname ptf-methods
## @aliases
setGeneric(name = "Portfolio2",
           def = function(...){
             standardGeneric("Portfolio2")
           })

## @include
#' @export
## @rdname ptf-methods
# @aliases
setMethod(f = "Portfolio2", signature = c(),
          definition = function(...){
            
            p <- new("Portfolio2")
            pars <- list(...)
            if ("source" %in% tolower(names(pars))) {
              source = pars[["source"]]
              pars[["source"]] = NULL
              print(
                import(object = new.portfolio, source = source, pars)
              )
            }
            p@.Data <- pars
            names(p@.Data) <- 
              unlist(lapply(p@.Data, function(x) x$ContractTerms$ContractID))
    # print(nam)
    #         print(names(p@.Data))
    #         names(p@.Data) <- nam
    #         print(names(p@.Data))
            return(p)
          })

## @include
#' @export
## @rdname ptf-methods
# @aliases
setMethod(f = "Portfolio2", signature = c("list"),
          definition = function(...){
            
            new.portfolio = new("Portfolio2")
            new.portfolio@.Data <- list(...)[[1]]
            return(new.portfolio)
          })


## @include
#' @export
#' @docType methods
## @rdname ptf-methods
## @aliases
setGeneric(name = "as.Portfolio2",
           def = function(...){
             standardGeneric("as.Portfolio2")
           })

## @include
#' @export
## @rdname ptf-methods
# @aliases
setMethod(f = "as.Portfolio2", signature = c("list"),
          definition = function(...){
            
            new.portfolio = new("Portfolio2")
            new.portfolio@.Data <- list(...)[[1]]
            return(new.portfolio)
          })



## @include
#' @export
setMethod("[", signature = c("Portfolio2", "ANY"),
          definition = function(x, i) {
            x@.Data <- x@.Data[i]
            x
          }
)

## @include
#' 
#' This version:
#' 1) Takes the risk factor connector of the first argument.
#' Question: is it possible to merge different risk factor environments?
#' 2) The events are lost.
#' It's difficult to collect them without an explicit loop over all
#' portfolios.
#' 
#' @export
#' 
setMethod("c", signature = c("Portfolio2"),
          definition = function(x, ...) {
            y <- new("Portfolio2", .Data=callNextMethod(),
                rf_connector = x@rf_connector)
            names(y) <- unlist(lapply(y@.Data, 
                                      function(x) x$ContractTerms$ContractID))
            y
          })


## @include
#' @export
setMethod("show", signature = c("Portfolio2"),
          definition = function(object) {
            print(names(object))
          }
)
