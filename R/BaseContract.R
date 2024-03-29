
#' @include ContractType.R
#' @export
setRefClass("BaseContract", 
            contains = "ContractType",
            fields = list(Dates = "character",
                          CashFlows = "numeric"))

##############################################################
#' \code{BaseContract}
#'
#' Constructor method for classes of type BaseContract representing cash flows. 
#' The class contains the fields 'Dates' and 'CashFlows', representing the 
#' dates, as well as values of the cash flows.
#' 
#' @param Dates a character vector representing dates.
#' 
#' @param CashFlows a numeric vector representing cash flow values
#'
#' @return a contract type of class BaseContract. 
#' 
#' @usage BaseContract(Dates, CashFlows, ...)
#' 
#' @examples
#' bc <- BaseContract(Dates = c("2019-12-31","2020-12-31"),
#'                    CashFlows = c(-90,100))
#'                    
#' @export
setGeneric(name = "BaseContract",
           def = function(...){
             standardGeneric("BaseContract")
           })

#' @export
setMethod(f = "BaseContract", signature = c(),
          definition = function(Dates = "0000-01-01",
                                CashFlows = NaN,
                                ...){
            pars <- list(Dates = Dates,
                         CashFlows = CashFlows,
                         ...)
            if (length(Dates)!=length(CashFlows)) {
              stop("ErrorIn::BaseContract:: Length of Dates and CashFlows must be the same !!!")
            }
            object <- new("BaseContract")
            object$Dates <- Dates
            object$CashFlows <- CashFlows
            return(object)
          })

#' @include YieldCurve.R Value.R
#' @export
setMethod("value", signature = c("BaseContract","character","missing","missing"),
          definition = function(object, by, compound = "continuous", period="Y", curve=YieldCurve(), ...){
            df_s <- discountFactors(curve, to=object$Dates, from=by, method=compound, period=period)
            # df <- data.frame(Date = by, Value = sum(object$CashFlows * df_s))
            Value = sum(object$CashFlows * df_s)
            dim (Value) = c(1, length(Value))
            colnames(Value) = as.character(by)
            # df <- data.frame(Date = by, )
            return(Value)
          })

#' @export
setMethod("show", signature = "BaseContract",
          definition = function(object){
            df <- data.frame(Dates = object$Dates,
                             CashFlows = as.numeric(object$CashFlows),
                             stringsAsFactors = FALSE)
            print(df, row.names=FALSE)
          })

