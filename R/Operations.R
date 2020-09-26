#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

## -----------------------------------------------------------------
#' Operations Contract class definition
#' 
#' An operations contract represents any operational activity 
#' in monetary units within an organization. Such an activity
#' can be an Investment, Reserving, or any (re-) occuring cost
#' or income position.
#' 
#' @param ContractID A unique identifier of the Operations contract
#' 
#' @param ContractDealDate Timestamp as per when the contract was initiated
#' 
#' @param Currency The currency in which \link{ReservingPattern}, \link{DepreciationPattern},
#'                 \link{IncomePattern} are denominated
#'                 
#' @param Params A list containing parameters used in the \link{CashFlowPattern}, 
#'               \link{InvestPattern}, and \link{ReservePattern} functions
#' 
#' @param ReservePattern A function evaluating the pattern of building reserves. The 
#'                       function must implemente two arguments:
#' \itemize{
#' \item{model}{The \link{RiskFactorConnector} object used when evaluating the pattern}
#' \item{params}{The \link{Params} object used when evaluating the pattern}
#' }
#' 
#' @param InvestPattern A function evaluating the pattern of investment and depreciation. The 
#'                       function must implemente two arguments:
#' \itemize{
#' \item{model}{The \link{RiskFactorConnector} object used when evaluating the pattern}
#' \item{params}{The \link{Params} object used when evaluating the pattern}
#' }
#' 
#' @param CashFlowPattern A function evaluating the pattern of generated cash flows. 
#'                        The function must implement two arguments:
#' \itemize{
#' \item{model}{The \link{RiskFactorConnector} object used when evaluating the pattern}
#' \item{params}{The \link{Params} object used when evaluating the pattern}
#' }
#' 
#' @param RiskFactorConnector (optional) A risk factor environment within which the Operations
#'                            contract will be evaluated
#' 
#' @return
#' 
#' @include FEMSContract.R
#' @export
#' @rdname ops-classes
setRefClass("Operations",
            contains = "FEMSContract",
            fields = list(
              ContractType = "character",
              ContractID = "character",
              ContractDealDate = "character",
              Currency = "character",
              RiskFactorConnector = "RiskFactorConnector"
            ))

## -----------------------------------------------------------------
# Child Classes of Operations & Constructorsv
#' @export
setRefClass("OperationalCF",
            contains = "Operations",
            fields = list(
              pattern = "function",
              args = "list"
            ))

## -----------------------------------------------------------------
#' OperationalCF Contract class definition
#' 
#' An operational cash flows contract represents any operational activity 
#' in monetary units within an organization. 
#' 
#' @param pattern A function evaluating the pattern of generated cash flows. 
#' 
#' @param args The list of arguments used when evaluating the pattern
#' 
#' @usage OperationalCF(ContractID, pattern, args, ...)
#' 
#' @examples 
#' times = timeSequence(from="2014-01-01", by="3 months", length.out=9)
#' values = cumsum(c(1,rnorm(8,0.02,0.1)))
#' idx <- Index(label = "PriceIndex", data = values, charvec = times)
#'
#' revenue <- function(idx, times) { 
#'   idx$Data[as.character(times),] * 1000
#' }
#' revenue(idx=idx, times=times)
#' OpCFs <- OperationalCF(
#'   ContractID="Ops001", Currency="CHF",
#'   pattern = revenue, 
#'   args = list( # the argument of the function
#'     idx = idx,  
#'    times = as.character(times)))
#' 
#' @export
setGeneric(name = "OperationalCF",
           def = function(...){
             standardGeneric("OperationalCF")
           })

#' @export
setMethod(f = "OperationalCF",signature = c(),
          definition = function(...){
            object = new("OperationalCF")
            pars = list(...)
            if(length(pars)==0){
            }  else if (is.list(pars[[1]])) {
              FEMS:::set(object=object, what=pars[[1]])
            } else {
              FEMS:::set(object=object, what=pars)
            }
            return(object)
          })

setMethod(f = "initialize", signature="OperationalCF",
          function(.Object, ...) {
            .Object <- callNextMethod()
            # initialize pars
            .Object$ContractType = "OperationalCF"
            .Object$pattern = function(model,params) { NULL }
            return(.Object)
          })


#' @export
#' @rdname ops-classes
setRefClass("Investments",
            contains = "Operations",
            fields = list(
              pattern = "function",
              args = "list"
            ))
## -----------------------------------------------------------------
#' Investments Contract class definition
#' 
#' An Investments contract represents any operational activity 
#' in monetary units within an organization related to investments.
#' 
#' @param pattern A function evaluating the pattern of generated investments. 
#' 
#' @param args The list of arguments used when evaluating the pattern
#' 
#' @usage Investments(ContractID, pattern, args, ...)
#' 
#' @examples 
#' times = timeSequence(from="2014-01-01", by="3 months", length.out=9)
#' write.off <- function(times) {
#'    timeSeries(seq(1000000, 0, length.out=9), times)
#'    }
#' invest <- Investments(
#'               ContractID = "Ops002", Currency = "CHF", 
#'               pattern = write.off, 
#'               args = list(times = times))
#' 
#' @export
setGeneric(name = "Investments",
           def = function(...){
             standardGeneric("Investments")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "Investments",signature = c(),
          definition = function(...){
            object = new("Investments")
            pars = list(...)
            if(length(pars)==0){
            }  else if (is.list(pars[[1]])) {
              FEMS:::set(object=object, what=pars[[1]])
            } else {
              FEMS:::set(object=object, what=pars)
            }
            return(object)
          })

setMethod(f = "initialize", signature="Investments",
          function(.Object, ...) {
            .Object <- callNextMethod()
            # initialize pars
            .Object$ContractType = "Investments"
            .Object$pattern = function(model,params) { NULL }
            return(.Object)
          })

#' @export
#' @rdname ops-classes
setRefClass("Reserves",
            contains = "Operations",
            fields = list(
              pattern = "function",
              args = "list"
            ))

#' @export
#' @rdname ops-methods
setGeneric(name = "Reserves",
           def = function(...){
             standardGeneric("Reserves")
           })

## @include
#' @export
#' @rdname ct-methods
setMethod(f = "Reserves",signature = c(),
          definition = function(...){
            object = new("Reserves")
            pars = list(...)
            if(length(pars)==0){
            }  else if (is.list(pars[[1]])) {
              FEMS:::set(object=object, what=pars[[1]])
            } else {
              FEMS:::set(object=object, what=pars)
            }
            return(object)
          })

setMethod(f = "initialize", signature="Reserves",
          function(.Object, ...) {
            .Object <- callNextMethod()
            # initialize pars
            .Object$ContractType = "Reserves"
            .Object$pattern = function(model,params) { NULL }
            return(.Object)
          })

## -----------------------------------------------------------------
#'
#'  Operations Contract constructor definition
# #' @include
#' @export
#' @rdname ops-methods
# setGeneric(name = "Operations",
#            def = function(...){
#              standardGeneric("Operations")
#            })

## @include
#' @export
# setMethod(f = "Operations",signature = c(),
#           definition = function(...){
#               object = new("Operations")
#               pars = list(...)
#               if(length(pars)==0){
#               }  else if (is.list(pars[[1]])) {
#                   FEMS:::set(object=object, what=pars[[1]])
#               } else {
#                 FEMS:::set(object=object, what=pars)
#               }
#               return(object)
#           })

## -----------------------------------------------------------------
## what happens when an instance should be created?
## @include 
## @export
## @rdname
# setMethod(f = "initialize", signature="Operations",
#           function(.Object, ...) {
#               .Object <- callNextMethod()
#               # initialize pars
#               .Object$ContractType = "Operations"
#               .Object$CashFlowPattern = function(model,params) { NULL }
#               .Object$InvestPattern = function(model,params) { NULL }
#               .Object$ReservePattern = function(model,params) { NULL }
#               return(.Object)
#           })

## -----------------------------------------------------------------
## get an overview of most important terms
## @include 
#' @export
## @rdname
# setMethod(f = "summary", signature = "Operations",
#           function(object){
#             # print all terms of the Operations contract
#             terms = FEMS:::get(object = object, what = "all")
#             print(terms)
#           })

## -----------------------------------------------------------------
## get list of all available terms
## @include 
#' @export
## @rdname
setMethod(f = "terms", signature = "Operations",
          function(object){
            return(grep("jref",names(object$getRefClass()$fields()),invert=TRUE,value=TRUE))
          })

## -----------------------------------------------------------------
## getter
## @include 
#' @export
## @rdname
setMethod(f = "get", signature = "Operations",
          function(object, what, ...){
            if(what=="all") what=FEMS:::terms(object)
            fields = sapply(what,function(x) object$field(x))
            return(fields)
          })

## -----------------------------------------------------------------
## setter
## @include 
#' @export
## @rdname
setMethod(f = "set", signature = c("Operations","list"),
          function(object, what, ...){
            silent = lapply(names(what),function(x) object$field(x,what[[x]]))
          })

#' @export
## @rdname
setMethod(f = "set", signature = c("Operations","RiskFactorConnector"),
          function(object, what, ...){
            object$RiskFactorConnector = what
          })

## -----------------------------------------------------------------
## add to portfolio
## @include 
#' @export
## @rdname
setMethod(f = "add", signature = c("Portfolio","Operations"),
          function(object, what, ...){
            object$contracts=c(object$contracts,what)
          })

## -----------------------------------------------------------------
## events methods for Operations contract
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "character", "missing"),
          definition = function(object, ad, model){
            return(FEMS:::events(object,timeDate(substring(ad,1,10))))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "AD0", "missing"),
          definition = function(object, ad, model){
            return(FEMS:::events(object, as.character(ad)))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "timeDate", "missing"),
          definition = function(object, ad, model){
            return(FEMS:::EventSeries(object,ad))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "character", "RiskFactorConnector"),
          definition = function(object, ad, model){
            return(FEMS:::events(object,timeDate(substring(ad,1,10)),model))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "AD0", "RiskFactorConnector"),
          definition = function(object, ad, model){
            return(FEMS:::events(object,as.character(ad),model))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Operations", "timeDate", "RiskFactorConnector"),
          definition = function(object, ad, model){
            set(object, model)
            return(events(object ,ad))
          })

#' ## -----------------------------------------------------------------
#' ## EventSeries methods for Operations contract
#' @export
#' @rdname evs-methods
setMethod(f = "EventSeries", signature = c("Operations", "character"),
          definition = function(object, ad, ...){
            EventSeries(object,timeDate(substring(ad,1,10)))
          })

setMethod(f = "EventSeries", signature = c("Operations", "AD0"),
          definition = function(object, ad, ...){
            EventSeries(object,as.character(ad))
          })

setMethod(f = "EventSeries", signature = c("Operations", "timeDate"),
          definition = function(object, ad, ...){

            # create event series object
            out <- new("EventSeries")
            out$id <- FEMS:::get(object,"ContractID")
            out$ct <- FEMS:::get(object,"ContractType")

            # AD0 event
            events <- data.frame(Date=as.character(ad),
                                Value=0.0,
                                Type="AD0",
                                Level="P",
                                Currency=object$Currency,
                                Time=0.0,
                                NominalValue=0.0,
                                NominalRate=0.0,
                                NominalAccrued=0.0)
            
            # evaluate cash flow pattern
            # ops <- object$CashFlowPattern(object$RiskFactorConnector, object$params)
            # code is generalized so that an arbitrary function with arbitrary
            # arguments can be passed.
            if (class(object)=="OperationalCF") {
              ops <- do.call(object$pattern, object$args)
            } else {
              ops <- NULL
            }
            if(!is.null(ops)) {
              vals <- as.numeric(series(ops))
              events <- rbind(events,
                           data.frame(Date=as.character(time(ops)),
                                      Value=c(vals[1],vals[2:length(vals)]),
                                      Type="OPS",
                                      Level="P",
                                      Currency=object$Currency,
                                      Time=yearFraction(as.character(ad), 
                                                        as.character(time(ops)), 
                                                        convention = "30E360"),
                                      NominalValue=0.0,
                                      NominalRate=0.0,
                                      NominalAccrued=0.0))
            }
            # evaluate invest pattern
            # Should be generalized, cf. above
            # ops <- object$InvestPattern(object$RiskFactorConnector,object$params)
            if (class(object)=="Investments") {
              ops <- do.call(object$pattern, object$args)
            } else {
              ops <- NULL
            }
            if(!is.null(ops)) {
              if (length(ops)<2) stop("An investment pattern needs to have length>1!")
              vals <- c(ops[1,],diff(ops)[-1,])
              events <- rbind(events,
                           data.frame(Date=as.character(time(ops)),
                                      Value=c(-vals[1],vals[2:length(vals)]),
                                      Type=c("IED",rep("DPR",length(ops)-1)),
                                      Level="P",
                                      Currency=object$Currency,
                                      Time=yearFraction(as.character(ad), 
                                                        as.character(time(ops)), 
                                                        convention = "30E360"),
                                      NominalValue=vals,
                                      NominalRate=0.0,
                                      NominalAccrued=0.0))
            }
            # If there is a salvage value (write-off no till 0)
            # we add a last event of type MD and the remaining value
            if ( tail(ops,1) > 0 ) {
              tmp <- tail(ops,1)
              events <- rbind(events,
                              data.frame(Date=as.character(time(tmp)),
                              Value=as.numeric(series(tmp)),
                              Type="MD",
                              Level="P",
                              Currency=object$Currency,
                              Time=yearFraction(as.character(ad), 
                                                as.character(time(tmp)), 
                                                convention = "30E360"), # This should not be hardcoded!!!
                              NominalValue=0,
                              NominalRate=0.0,
                              NominalAccrued=0.0))
            }
            # evaluate reserving pattern
            # Should be generalized, cf. above
            if (class(object)=="Reserves") {
              ops <- object$pattern(object$RiskFactorConnector, object$args)
            } else {
              ops <- NULL
            }
            if(!is.null(ops)) {
              # compute change in nominal value, note, reserves are liabilities so interprete
              # nominal positions as (-1) * nominal
              vals <- diff(-ops)[-1]
              events <- rbind(events,
                           data.frame(Date=as.character(time(ops))[-1],
                                      Value=vals,
                                      Type="RES",
                                      Level="P",
                                      Currency=object$Currency,
                                      Time=yearFraction(as.character(ad), 
                                                        as.character(time(ops))[-1], 
                                                        convention = "30E360"),
                                      NominalValue=vals,
                                      NominalRate=0.0,
                                      NominalAccrued=0.0))
            }
            
            # convert to (sorted) timeSeries
            # Note: AD0 event needs to be after all other events of the same instant
            tms <- paste0(events$Date,"T00:00:00")
            tms[events$Type=="AD0"] <- paste0(substring(tms[events$Type=="AD0"],1,10),"T23:59:59")
            events <- events[order(tms),]
            evs.ts <- timeSeries(events,timeDate(events$Date))
            
            # compute nominal value
            evs.ts$NominalValue <- cumsum(evs.ts$NominalValue)
            
            # exclude pre-ad0 events
            # Note, its a sorted series so just look for AD0-event index
            evs.ts <- tail(evs.ts,nrow(evs.ts)-(which(evs.ts$Type=="AD0")-1))
            
            # convert back to data.frame
            events <- as.data.frame(series(evs.ts))
            events$Value <- as.numeric(events$Value)
            events$Time <- as.numeric(events$Time)
            events$NominalValue <- as.numeric(events$NominalValue)
            events$NominalRate <- as.numeric(events$NominalRate)
            events$NominalAccrued <- as.numeric(events$NominalAccrued)
            rownames(events) <- NULL
            
            # attach events to series
            out$evs  <-  events
            
            return(out)
          })

## -----------------------------------------------------------------
## liquidity methods for Operations contract
#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Operations", "timeDate", "missing"),
          definition = function(object, by, type, ...){
            return(liquidity(object, by, type="marginal", ...))
          })

#' @export
#' @include TimeBuckets.R
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Operations", "timeBuckets", "missing"),
          definition = function(object, by, type, ...){
            liq <- liquidity(object, as.timeDate(by), type="marginal", ...)
            names(liq) <- by@bucketLabs
            return(liq)
          })

#' @export
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Operations", "timeDate", "character"),
          definition = function(object, by, type, ...){
            ops.liquidity(events(object, by[1]), by, type, ...)
          })

#' @export
#' @include TimeBuckets.R
#' @rdname liq-methods
setMethod(f = "liquidity", signature = c("Operations", "timeBuckets", "character"),
          definition = function(object, by, type, ...){
            liq <- liquidity(object, as.timeDate(by), type=type, ...)
            names(liq) <- by@bucketLabs
            return(liq)
          })

# internal liquidity calculation function
#
# @param object a data.frame, the output of events method applied to an Operations contract
# @param by the by argument to a liquidity method
# @param type the type argument to a liquidity method
ops.liquidity = function(object, by, type, digits=2){
            if (type=="marginal") {
              evs = FEMS:::get(object, "evs")
              # filter by liquidity-category events
              evs = subset(evs, Type %in% c("AD0","OPS","PR"))
              # compute aggregate cash flows for remaining events
              liq = timeSeries(rep(0, length(by)), charvec=by)
              cf.raw = timeSeries(evs$Value,
                                charvec=substring(evs$Date, 1, 10))
              cf.aggr = aggregate(cf.raw, by, FUN=sum)
              liq[time(cf.aggr),] <- cf.aggr
              liq = as.numeric(series(liq))[-1]
            } else if (type=="cumulative") {
              liq = cumsum(liquidity(object, by, type="marginal"))
            } else {
              stop(paste("Liquidity type '", type, "' not recognized!", sep=""))
            }
            return(round(liq, digits))
}

#' ## -----------------------------------------------------------------
#' ## income methods for Operations contract
#' #' @export
#' #' @rdname inc-methods
#' setMethod(f = "income", signature = c("Operations", "timeDate", "missing"),
#'           definition = function(object, by, type, ...){
#'             return(income(object, by, type="marginal", ...))
#'           })
#' 
#' #' @export
#' #' @include TimeBuckets.R
#' #' @rdname inc-methods
#' setMethod(f = "income", signature = c("Operations", "timeBuckets", "missing"),
#'           definition = function(object, by, type, ...){
#'             inc <- income(object, as.timeDate(by), type="marginal", ...)
#'             names(inc) <- by@bucketLabs
#'             return(inc)
#'           })
#' 
#' 
#' #' @export
#' #' @rdname liq-methods
#' setMethod(f = "income", signature = c("Operations", "timeDate", "character"),
#'           definition = function(object, by, type, ...){
#'             rflSimulation:::ops.income(object, by, type, ...)
#'           })
#' 
#' #' @export
#' #' @include TimeBuckets.R
#' #' @rdname inc-methods
#' setMethod(f = "income", signature = c("Operations", "timeBuckets", "character"),
#'           definition = function(object, by, type, ...){
#'             inc <- income(object, as.timeDate(by), type=type, ...)
#'             names(inc) <- by@bucketLabs
#'             return(inc)
#'           })
#' 
#' 
#' # internal income calculation function
#' #
#' # @param object an Operations contract
#' # @param by the by argument to a income method
#' # @param type the type argument to a income method
#' ops.income = function(object, by, type, digits=2){
#'   if (!type %in% c("marginal", "cumulative")) {
#'     stop(paste("Income type '", type, "' not recognized!", sep=""))
#'   }
#'   
#'   # compute events
#'   events <- events(object, by[1])
#'   
#'   # compute marginal income
#'   evs <- FEMS:::get(events, "evs")
#'   # filter by income-category events
#'   evs <- subset(evs, Type %in% c("AD0", "OPS", "DPR", "RES"))
#'   # compute aggregate cash flows for remaining events
#'   inc <- timeSeries(rep(0, length(by)), charvec=by)
#'   cf.raw <- timeSeries(evs$Value,
#'                     charvec=substring(evs$Date, 1, 10))
#'   cf.aggr=aggregate(cf.raw, by, FUN=sum)
#'   inc[time(cf.aggr),] <- cf.aggr
#'   inc <- as.numeric(series(inc))[-1]
#'   
#'   # compute cumulative
#'   if(type=="cumulative") {
#'     inc <- cumsum(inc)
#'   }
#'     
#'   return(round(inc, digits))
#' }


## -----------------------------------------------------------------
## value methods for Operations Contract
#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "AD0", "character", "missing"),
          definition = function(object, by, type, ... ){
            value(object, timeDate(substring(as.character(by), 1, 10)),type, ...)       
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "character", "character", "missing"),
          definition = function(object, by, type, ...){
            value(object,timeDate(substring(by, 1, 10)), type, ...)
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "timeDate", "character", "missing"),
          definition = function(object, by, type, ...){
            if(type=="nominal") {
              return(ops.nominal(events(object, by[1]), by, ...))
            } else if (type %in% c("markToModel","markToMarket")) {
              stop("Need argument 'method' in order to evaluate 'markToModel'-type value!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "timeBuckets", "character", 
                                     "missing"),
          definition = function(object, by, type, ...){
            # message("Method 'value' with signature with timeBuckets")
            if(type=="nominal") {
              by2 = as.timeDate(by)
              val = ops.nominal(events(object, by2[1]), by2, ...)
              names(val) = by@breakLabs
              return(val)
            } else if (type %in% c("markToModel","markToMarket")) {
              stop("Need argument 'method' in order to evaluate 'markToModel'-type value!")
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
          })


setMethod(f = "value", signature = c("Operations", "AD0", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            value(object,timeDate(substring(as.character(by), 1, 10)), type, method, ...)       
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "character", "character", "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            value(object, timeDate(substring(by, 1, 10)), type, method, ...)
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "timeDate", "character", 
                                     "DiscountingEngine"),
          definition = function(object, by, type, method, ...){
            if(type=="nominal") {
              return(ops.nominal(events(object, by[1]), by, ...))
            } else if (type %in% c("markToModel","markToMarket")) {
              # print("valuation of operations contract")
              return(ops.marketValue(events(object, by[1]), by, method, ...))
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
          })

#' @export
#' @rdname val-methods
setMethod(f = "value", signature = c("Operations", "timeBuckets", "character", 
                                     "DiscountingEngine"),
          definition = function(object, by, type, method, ...){

            # message("Method 'value' with signature with timeBuckets")
            if(type=="nominal") {
              by2 = as.timeDate(by)
              val = ops.nominal(events(object, as.timeDate(by2[1])[1]), by2, ...)
              names(val) = by@breakLabs
              return(val)
            } else if (type %in% c("markToModel","markToMarket")) {
              val = ops.marketValue(events(object, as.timeDate(by[1])[1]), by, method, ...)
              names(val) = by@breakLabs
              return(val)
            } else {
              stop(paste("Value type '", type, "' not recognized!", sep=""))
            }
          })


#-------------------------------------------------
# internal value functions
ops.nominal = function(object, by, digits=2) {
  # message("entered ops.nominal")
  # extract events and times
  evs <- FEMS:::get(object, "evs")[,c("Date", "NominalValue", "Type")]
  colnames(evs) <- c("times","values","types")
  evs$times <- timeDate(evs$times)
  
  # message("execute sapply") 
  # iterate through valuation times and fetch "last observed" nominal value state
  val <- sapply(substring(by, 1, 10), function(ad) {
    # evs.sub <- subset(evs, times<=timeDate(substring(ad, 1, 10)))
    # "<=" is ok for T00 but for T24, it shoud be "<".
    evs.sub <- subset(evs, times<=timeDate(ad))
    return(tail(evs.sub, n=1)$values)
  })
  return(round(val, digits=digits))
}

ops.marketValue = function(object, by, method, digits=2) {
 
  # extract discounting parameters
  spread <- FEMS:::get(method,"DiscountingSpread")
  dc <- get(method, "RiskFactorObject")
  FEMS::set(dc, list(Rates = FEMS::get(dc, "Rates") + spread))
  
  # extract cashflow events
  evs <- FEMS:::get(object,"evs")[, c("Date", "Value","Type")]
  colnames(evs) <- c("times", "values", "types")
  evs$times <- timeDate(evs$times)
  evs <- subset(evs, types %in% c("OPS", "PR"))
  
  # iterate through valuation times and compute present value of remaining 
  # cashflows
  by <- as.character(by)
  val <- sapply(by, function(ad) {
    # times must be STRICTLY greater (">"), otherwise inconsistent with 
    # liquidity and income computation
    evs.sub <- subset(evs, times > timeDate(substring(ad, 1, 10)))
    if (nrow(evs.sub)==0) {
      return(0.0)
    } else {
      cfs <- evs.sub$values
      dts <- as.character(evs.sub$times)
      dfs <- FEMS::discountFactors(dc, start=ad, end=dts, 
                                     isDateEnd=TRUE)
      return(as.numeric(cfs%*%dfs))
    }
  })
  
  # rebase yield curve
  set(dc, 
      list(Rates=FEMS:::get(dc,"Rates") - spread)) # TODO: implement discounting more consistently
  
  # return values
  return(round(val, digits=digits))
}