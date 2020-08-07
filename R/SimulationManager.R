# @include Portfolio.R RiskFactorConnector.R PortfolioTree.R
#' @export
setRefClass("SimulationManager",
            fields = list(Portfolio = "Portfolio",
                          ModelStructure = "PortfolioTree",
                          RiskFactors = "RiskFactorConnector",
                          TimeBuckets = "timeBuckets",
                          Strategy = "matrix",
                          Templates = "list"))

#' @export
setGeneric(name = "SimulationManager",
           def = function(...){
             standardGeneric("SimulationManager")
           })

#' @export
setMethod(f = "SimulationManager", signature = c(),
          definition = function(...){
            object <- new("SimulationManager")
            pars <- list(...)
            set(object = object, what = pars)
            return(object)
          })

#' @export
setMethod(f = "set", signature = c("SimulationManager","list"),
          definition = function(object, what){
            object <- new("SimulationManager")
            for (i in names(what)) {
              if (is.valid.sm.field(i)) {
                value <- what[[i]]
                switch(i,
                       Portfolio = {
                         object$Portfolio <- value
                       },
                       TreeStructure = {
                         object$TreeStructure <- value
                       },
                       RiskFactors = {
                         object$RiskFactors <- value
                       },
                       TimeBuckets = {
                         object$TimeBuckets <- value
                       },
                       Strategy = {
                         object$Strategy <- value
                       },
                       Templates = {
                         object$Templates <- value
                       })
              } else {
                warning(paste("ErrorIn::SimulationManager:: Field ", i, " does not exist, cannot assign value!", sep = ""))
              }}
            return(object)
          })

#' @export
setGeneric(name = "simulate",
           def = function(object, start_date, end_date, ...){
             standardGeneric("simulate")
           })

#' @export
setMethod(f = "simulate", signature = c("SimulationManager","list"),
          definition = function(object, start_date, end_date, ...){
            
            
            
            
          })

# check if fields are valid
is.valid.sm.field <- function(x) {
  valid <- c("Portfolio", "TreeStructure","RiskFactors","TimeBuckets","Strategy","Templates")
  return(x %in% valid)
}



