
#' @include Portfolio.R RiskFactorConnector.R ModelStructure.R
#' @export
#' @rdname ctm-classes
FinancialModel <- setRefClass("FinancialModel",
                          fields = list(
                            mstructure = "Node",
                            treasury = "Node",
                            curr_acc = "CurrentAccount",
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
  simulate = function(t.start, t.end, by, ...) {
## 'Executes the simulation of a financial model.'
    if (is.character(t.start)) t.start <- as.timeDate(t.start)
    if (is.character(t.end)) t.end <- as.timeDate(t.end) 
    tSeq = timeSequence(from=t.start, to=t.end, by=by)
    # print(tSeq)
    # Initialization
    clearEvents(mstructure)
    tt = tSeq[1]
    # print(paste("Simulation start:", tt, ad0))
    events(mstructure, ad0, rf, as.character(tt))
    # print(mstructure$Expenses$eventList)
    liq <- liquidity(mstructure, buckets, type="marginal")
    # print(liq)
    val <- value(mstructure, buckets, type="nominal")
    # print(val)
    iam <- data.frame(as.numeric(liquidity(mstructure, buckets, "marginal")[1,]),
                      row.names=as.character(buckets)[-1])
    names(iam) <- "InternalCashFlows"
    # print("iam, all dates:")
    # print(iam)
    iam <- iam[row.names(iam)<=as.character(tt),,drop=FALSE]
    # print(iam)
    # print(dim(iam))
    # print(paste("length of iam:", iam))
    # id <- treasury$contracts$CurrAcc$ContractID
    # print(id)
    # if (dim(iam)[1]) { curr_acc$InternalCashFlows <<- iam  } ## Die add Methode liefert Unsinn
    if (dim(iam)[1]) { add.internalcashflow(curr_acc, iam)  } ## Die add Methode liefert Unsinn
    
    id <- curr_acc$ContractID
    # print(paste("id =", id))
    # print("------------------------------------------------")
    # print(paste("Simulation loop"))
    for (i in 2:length(tSeq)) {
      tt = tSeq[i]
      print(paste(i, as.character(tt)))
      # The following steps must be carried out:
      #   1. Update variable contract terms except current account,
      #      in particular Operations contracts.
      
      #   2. Generate new contracts as defined by strategy matrix

            #   3. Compute Contract Events except for current account
      # liq <- liquidity(mstructure, buckets, type="marginal")
      # print(liq)
      # val <- value(mstructure, buckets, type="nominal")
      # print(val)
      #   4. Update ContractTerm "CashFlows" of current account with
      #      marginal liquidity at the end of current time step.
      iam <- data.frame(as.numeric(liquidity(mstructure, buckets, "marginal")[1,]),
                       row.names=as.character(buckets)[-1])
      names(iam) <- "InternalCashFlows"
      ## Das richtige Zeitfenster auswählen
      ids <- row.names(iam)<=as.character(tt) & row.names(iam)>as.character(tSeq[i-1])
      # print(ids)
      iam <- iam[ids,,drop=FALSE]  
      # print(iam)
      # print(paste("length of iam:", length(iam)))
      # if (dim(iam)[1]) { curr_acc$InternalCashFlows <<- iam  } ## Die add Methode liefert Unsinn
      if (dim(iam)[1]) { add.internalcashflow(curr_acc, iam)  } ## Die add Methode liefert Unsinn
      # print(mstructure$Wealth$contracts[[id]]$InternalCashFlows)
      
      # print(mstructure$Active$Treasury$contracts$CurrAcc)
      events(mstructure, ad0, rf, as.character(tt))
      #  5.  Compute ContractEvents for current account
      evL <- eventList()
      evL[[id]] <- events(curr_acc, ad0, rf, as.character(tt))
      # print(evL)
      treasury$eventList <<- evL
      # print(treasury$eventList)
      # print(value(evL[[1]], buckets, type="nominal"))
      # print(value(mstructure$Wealth, buckets, type="nominal"))
      liq <- liquidity(mstructure, buckets, type="marginal")
      # print(liq)
      val <- value(mstructure, buckets, type="nominal")
      # print(val)
      # print(treasury$eventList)
      # print(treasury$eventList)
      # print(treasury$contracts[[id]]$InternalCashFlows)
      # print(mstructure$Expenses$eventList)
      # print(value(mstructure, buckets, type="nominal"))
      # print("------------------------------------------------")
    }
  }
)

# check if fields are valid
is.valid.sm.field <- function(x) {
  valid <- c("ModelStructure", "RiskFactors","TimeBuckets", "SimulationSteps", 
             "Strategy","Templates")
  return(x %in% valid)
}



