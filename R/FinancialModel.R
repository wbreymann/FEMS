# @include Portfolio.R RiskFactorConnector.R ModelStructure.R
#' @export
setRefClass("SimulationManager",
            fields = list(ModelStructure = "Node",
                          RiskFactors = "RiskFactorConnector",
                          TimeBuckets = "timeBuckets",
                          SimulationSteps = "character",
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
            pars = list(...)
            if ( length(pars)>0 ) {
              pars <- list(...)
              set(object, what=pars)
            }
             return(object)
          })

#' @export
setMethod(f = "set", signature = c("SimulationManager"),
          definition = function(object, ...){
            for (i in names(what)) {
              if (is.valid.sm.field(i)) {
                value <- what[[i]]
                switch(i,
                       ModelStructure = {
                         object$ModelStructure <- value
                       },
                       RiskFactors = {
                         object$RiskFactors <- value
                       },
                       TimeBuckets = {
                         object$TimeBuckets <- value
                       },
                       SimulationSteps = {
                         object$SimulationSteps <- value
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
           def = function(object, start, end, by, ...){
             standardGeneric("simulate")
           })

#' Carries out the simulation.
#' For every Object, in creates an EventSeries that is the basis for all further
#' analytics.
#' @export
setMethod(f = "simulate", 
          signature = c("SimulationManager", "timeDate", "timeDate", "character"),
          definition = function(object, start, end, by, ...) {
          
          tSeq = timeSequence(from=start, to=end, by=by)
          
          for (tt in as.character(tSeq)) {
            # The following steps must be carried out:
            #   1. Update variable contract terms except current account,
            #      in particular Operations contracts.
            #   2. Generate new contracts as defined by strategy matrix 
            #   3. Compute Contract Events except for current account
            #   4. Update ContractTerm "CashFlows" of current account with 
            #      marginal liquidity at the end of current time step.
            #  5.  Compute ContractEvents for current account
            
          }
            
         
            
            
          })

# check if fields are valid
is.valid.sm.field <- function(x) {
  valid <- c("ModelStructure", "RiskFactors","TimeBuckets", "SimulationSteps", 
             "Strategy","Templates")
  return(x %in% valid)
}



