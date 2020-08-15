
#' @include Portfolio.R RiskFactorConnector.R ModelStructure.R
#' @export
#' @rdname ctm-classes
FinancialModel <- setRefClass("FinancialModel",
                          fields = list(
                            mstructure = "Node",
                            rf = "RiskFactorConnector",
                            buckets = "timeBuckets",
                            ad0 = "character",
                            steps = "character",
                            strategy = "matrix",
                            templates = "list"
                          )
                  )

# #' @export
 # setGeneric(name = "FinancialModel",
 #           def = function(...){
 #             standardGeneric("FinancialModel")
 #           })

# #' @export
# setMethod(f = "FinancialModel", signature = c(),
#           definition = function(...){
#             object <- new("FinancialModel")
#             pars = list(...)
#             if ( length(pars)>0 ) {
#               pars <- list(...)
#               set(object, what=pars)
#             }
#              return(object)
#           })

# #' @export
# setMethod(f = "set", signature = c("FinancialModel"),
#           definition = function(object, ...){
#             for (i in names(what)) {
#               if (is.valid.sm.field(i)) {
#                 value <- what[[i]]
#                 switch(i,
#                        ModelStructure = {
#                          object$ModelStructure <- value
#                        },
#                        RiskFactors = {
#                          object$RiskFactors <- value
#                        },
#                        TimeBuckets = {
#                          object$TimeBuckets <- value
#                        },
#                        SimulationSteps = {
#                          object$SimulationSteps <- value
#                        },
#                        Strategy = {
#                          object$Strategy <- value
#                        },
#                        Templates = {
#                          object$Templates <- value
#                        })
#               } else {
#                 warning(paste("ErrorIn::FinancialModel:: Field ", i, " does not exist, cannot assign value!", sep = ""))
#               }}
#             return(object)
#           })

# #' @export
# setGeneric(name = "simulate",
#            def = function(object, start, end, by, ...){
#              standardGeneric("simulate")
#            })

# #' Carries out the simulation.
# #' For every Object, in creates an EventSeries that is the basis for all further
# #' analytics.
# #' @export

#' Simulate method
#' @export
FinancialModel$methods(
  simulate = function(start, end, by, ...) {
## 'Executes the simulation of a financial model.'
    if (is.character(start)) start <- as.timeDate(start)
    if (is.character(end)) end <- as.timeDate(end) 
    tSeq = timeSequence(from=start, to=end, by=by)
    print(tSeq)
    # Initialization
    tt = tSeq[1]
    print(tt)
    for (i in 2:length(tSeq)) {
      tt = tSeq[i]
      print(paste(i, tt))
      # The following steps must be carried out:
      #   1. Update variable contract terms except current account,
      #      in particular Operations contracts.
      
      #   2. Generate new contracts as defined by strategy matrix

            #   3. Compute Contract Events except for current account
      events(mstructure, ad0, rf, as.character(tt))
      liq <- liquidity(mstructure, buckets, type="marginal")
      print(liq)
      val <- value(mstructure, buckets, type="nominal")
      print(val)
      #   4. Update ContractTerm "CashFlows" of current account with
      #      marginal liquidity at the end of current time step.
      iam <- data.frame(as.numeric(liquidity(mstructure, buckets, "marginal")[1,]),
                       row.names=as.character(buckets)[-1])
      names(iam) <- "InternalCashFlows"
      iam <- iam[row.names(iam)<=as.character(tt),,drop=FALSE]
      print(iam)
      id <- mstructure$Active$Treasury$contracts$CurrAcc$ContractID
      mstructure$Active$Treasury$contracts[[id]]$InternalCashFlows <<- iam  ## Die add Methode liefert Unsinn

      # print(mstructure$Active$Treasury$contracts$CurrAcc)
      #  5.  Compute ContractEvents for current account
      evL <- eventList()
      evL[[id]] <- events(mstructure$Active$Treasury$contracts[[id]], ad0, rf, as.character(tt))
      print(evL)
      mstructure$Active$Treasury$eventList <<- evL
      liq <- liquidity(mstructure, buckets, type="marginal")
      print(liq)
      val <- value(mstructure, buckets, type="nominal")
      print(val)
    }
  }
)

# check if fields are valid
is.valid.sm.field <- function(x) {
  valid <- c("ModelStructure", "RiskFactors","TimeBuckets", "SimulationSteps", 
             "Strategy","Templates")
  return(x %in% valid)
}



