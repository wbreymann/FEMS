#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#
# An auxiliary class mimicking a simple list but for use
# with EventSeries-elements. The class and constructor are
# not exported since only used internally and in particular
# in order to override standard functions such as "as.data.frame"
eventList=setClass("eventList", slots=list(), contains="list")

##############################################################
#' A Reference Class representing a (Time-) Series of 
#' Contract Events
#' 
#' This class represents a structure collection of Contract
#' Events. Events are ordered on the timeline according to 
#' their Event Date and Sequence (cf. ACTUS documentation). 
#' 
#' @field jref A rJava java object reference 
#' 
#' @seealso \code{\link{generateEvents, processEvents}}
#'
## @examples
#' 
## @include
#' @export 
#' @rdname evs-classes
setRefClass("EventSeries",
            fields = list(
              evs = "data.frame",
              id = "character",
              ct = "character")
)

##############################################################
#' \code{EventSeries}-class constructor
#'
#' Create an instance of class \code{EventSeries}
#' 
#' Optionally, a \code{ContractType}-object can be passed on to 
#' the constructor in which case the java 
#' reference to object's 'processed event series' is
#' attached as the classes' \code{jref}-field. 
#' 
#' If no argument is provided, 
#' then a new empty java event series is created and 
#' referenced to by the classe's \code{jref} field.
#' 
#' @param object (optional) A \code{ContractType}-object from which to extract the EventSeries
#'
#' @return An object of a class \code{EventSeries} containing a
#'         reference to a java EventSeries object 
#' 
#' @seealso \code{\link{ContractType, generateEvents, processEvents}}
#'
#' @examples 
#' # example 1: create a new, empty object
#' # evs = EventSeries() # this constructor doesn't exist
#' 
#' # example 2: create a new object and attach a contract's Event
#  #            Series
#' pam = Pam()
#' set(pam, what=list(
#'                    ContractID = "001",
#'                    Currency = "CHF",
#'                    ContractRole = "RPA",
#'                    StatusDate       = "2012-12-31T00",
#'                    ContractDealDate = "2012-12-31T00",
#'                    InitialExchangeDate = "2013-01-01T00",
#'                    MaturityDate = "2013-03-31T00",
#'                    NotionalPrincipal = 1000, 
#'                    NominalInterestRate = 0.01,
#'                    DayCountConvention = "30E/360"))
#' ad = "2012-12-31T00"
#' evs = EventSeries(pam, ad)
#'
## @include
#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries,missing-method
#' @aliases EventSeries,ContractType-method
setGeneric(name = "EventSeries",
           def = function(object, ad, ...){
             standardGeneric("EventSeries")
           })

#' @include AnalysisDate.R
#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries, missing-method
setMethod(f = "EventSeries", signature = c("ContractType","timeDate"),
          definition = function(object, ad, ...){
            port <- Portfolio()
            add(port, object)
            set(port, object$rf_connector)
            EventSeries(port, AD0(ad))
          })

#' @include AnalysisDate.R
#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries, missing-method
setMethod(f = "EventSeries", signature = c("ContractType","AD0"),
          definition = function(object, ad, ...){
            port <- Portfolio()
            add(port, object)
            set(port, object$rf_connector)
            EventSeries(port, ad)
          })

#' @include AnalysisDate.R
#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries, missing-method
setMethod(f = "EventSeries", signature = c("ContractType", "character"),
          definition = function(object, ad, ...){
            port <- Portfolio()
            add(port, object)
            set(port, object$rf_connector)
            EventSeries(port, AD0(ad))
          })


#' @include AnalysisDate.R
#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries, missing-method
setMethod(f = "EventSeries", signature = c("Portfolio", "AD0"),
          definition = function(object, ad, ...){

            # compute events
            evs_raw <- generateEvents(object, ad)
            evs_list <- list()
            ct_list <- list()
            id_list <- list()
            for (i in 1:length(evs_raw)) {
              types <- getEventAttributes(evs_raw[[i]]$events, "type")
              payoff <- getEventAttributes(evs_raw[[i]]$events, "payoff")
              # payoff[types %in% c("IPCI","PRY","CD","RR","RRY","SC","IPCB")] = 0
              time <- getEventAttributes(evs_raw[[i]]$events, "time")
              temp_df <- data.frame(
                # ContractID = evs_raw[[i]]$contractId,
                Date = substring(time, 1, 10),
                Value = payoff,
                Type = types,
                Currency = getEventAttributes(evs_raw[[i]]$events, "currency"),
                Time = yearFraction(substring(time[1], 1, 10), substring(time, 1, 10), convention = "A/365"),
                NominalValue = getEventAttributes(evs_raw[[i]]$events, "nominalValue"),
                NominalRate = getEventAttributes(evs_raw[[i]]$events, "nominalRate"),
                NominalAccrued = getEventAttributes(evs_raw[[i]]$events, "nominalAccrued"))
              evs_list[[i]] <- temp_df
              id_list[[i]] <- evs_raw[[i]]$contractId
              ct_list[[i]] <- object$contracts[[evs_raw[[i]]$contractId]]$ContractTerms$ContractType
            }
            
            if (length(evs_raw)==1){
              out <- new("EventSeries")
              out$evs <- evs_list[[1]]
              out$id <- unlist(id_list)
              out$ct <- unlist(ct_list)
            } else {
              out <- eventList()
              for (j in 1:length(evs_raw)) {
                temp <- new("EventSeries")
                temp$evs <- evs_list[[j]]
                temp$id <- id_list[[j]]
                temp$ct <- ct_list[[j]]
                out[[as.character(temp$id)]] <- temp
              }
            }
            return(out)
          })

#' @export
#' @docType methods
#' @rdname evs-methods
#' @aliases EventSeries, missing-method
setMethod(f = "EventSeries", signature = c("EventSeries", "missing"),
          definition = function(object, ad, ...){
            x <- new("EventSeries")
            x$evs <- object$evs
            x$id <- object$id
            x$ct <- object$ct
            x
          })


##############################################################
#' \code{EventSeries}-print method
#'
#' Prints an \code{EventSeries} 
#' 
#' This method prints an object of class \code{EventSeries} in an formatted way.
#' 
#' Formatting options are provided
#' each event's EventDate, EventType, EventValue, and state 
#' variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object.
#' 
#' @param x An object of class \code{EventSeries} 
#'        
#'
#' @return An R-data.frame containing a the structured printout
#' 
#' @seealso \code{\link{ContractType, events}}
#'
#' @examples 
#' pam = Pam()
#' set(pam, what=list(
#'                    ContractID = "001",
#'                    Currency = "CHF",
#'                    ContractRole = "RPA",
#'                    StatusDate       = "2012-12-31T00",
#'                    ContractDealDate = "2012-12-31T00",
#'                    InitialExchangeDate = "2013-01-01T00",
#'                    MaturityDate = "2013-03-31T00",
#'                    NotionalPrincipal = 1000, 
#'                    NominalInterestRate = 0.01,
#'                    DayCountConvention = "30E/360"))
#' ad = AD0("2012-12-31T24")
#' evs = events(pam, ad)
#' class(evs)
#' 
#' print(evs)
#' print(evs, indices=c(-4,-9))
#'
## @include
#' @export
#' @docType methods
#' @rdname print-methods
setMethod("print", signature = "EventSeries", 
          definition = function(x, type = "pretty", indices, ...) {
            
            if (type == "raw") {
              x
            } else {
              y = FEMS::get(object=x, what="evs")
              if (!missing(indices)){ 
                y = y[,indices] 
              }
              if (type == "pretty") { 
                colnames(y) = .defaults$shortNames[colnames(y)]
                for (nam  in colnames(y))
                {
                  if (is.numeric(y[,nam])) {
                    y[,nam] = round(y[,nam],4)
                  }
                }
              }
              y$Date <- as.vector(sapply(y$Date, function(x) 
                              check.date.format(x, add = FALSE)))
              y
            }
          }
)


##############################################################
#' \code{EventSeries}-show method
#'
#' displays an \code{EventSeries} in terminal
#'
#' This method displays an object of class \code{EventSeries} in an formatted way.
#'
#' Each event's EventDate, EventType, EventValue, and state
#' variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object.
#'
#' @param object An object of class \code{EventSeries}
#'
#'
#' @return show returns an invisible NULL.
#'
#' @seealso \code{\link{ContractType, events}}
#'
#' @examples
#' pam = Pam()
#' set(pam, what=list(
#'                    ContractID = "001",
#'                    Currency = "CHF",
#'                    ContractRole = "RPA",
#'                    StatusDate       = "2012-12-31T00",
#'                    ContractDealDate = "2012-12-31T00",
#'                    InitialExchangeDate = "2013-01-01T00",
#'                    MaturityDate = "2013-03-31T00",
#'                    NotionalPrincipal = 1000,
#'                    NominalInterestRate = 0.01,
#'                    DayCountConvention = "30E/360"))
#' ad = AD0("2012-12-31T24")
#' events(pam, ad)
#'
#'
## @include
#' @export
#' @docType methods
#' @rdname show-methods
setMethod("show", signature = "EventSeries",
          definition = function(object) {
            print( get(object = object, what = "evs"))
            cat(paste0("Contract type: ", object$ct, "\n"))
            cat(paste0("Id: ",object$id, "\n"))
          }
)


##############################################################
#' \code{EventSeries}-subscript methods
#'
#' accesses elements of an \code{EventSeries}
#'
#' This method prints an object of class \code{EventSeries} in an formatted way.
#' Formatting options are provided
#'
#' each event's EventDate, EventType, EventValue, and state
#' variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object.
## @include
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("EventSeries", "character", "missing"),
          definition = function(x, i) {
            id <- is.element(x$evs[,1], i)
            x[id, 1:length(x$evs)]
          }
)

## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("EventSeries", "numeric", "missing"),
          definition = function(x, i) {
            x[i, 1:length(x$evs)]  
          }
)

## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("EventSeries", "numeric", "numeric"),
          definition = function(x, i, j) {
            y <- x$evs[i,j]
            if (!is.data.frame(y)) {
              y <- as.data.frame(y)
              rownames(y) <- rownames(x$evs)[i]
              colnames(y) <- colnames(x$evs)[j]
            }
            z <- new("EventSeries")
            z$evs <- y
            z$id <- x$id
            z$ct <- x$ct
            z
          }
)

## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("EventSeries", "numeric", "logical"),
          definition = function(x, i, j) {
            id <- (1:length(x$evs))[j]
            x[i, id]
          }
)


## @include 
#' @export
#' @docType methods
#' @rdname subscript-methods
setMethod("[", signature = c("EventSeries", "numeric", "character"),
          definition = function(x, i, j) {
            id <- is.element(colnames(x$evs), j)
            x[i, id]
          }
)

#' ## @include 
#' #' @export
#' #' @docType methods
#' #' @rdname subscript-methods
#' setMethod("[<-", signature = c("EventSeries", "numeric", "numeric", "ANY"),
#'           definition = function(x, i, j, y) {
#'             x$evs[i,j] = y
#'             x
#'           }
#' )


##############################################################
#' \code{EventSeries}-to-data.frame method
#'
#' Convert an \code{EventSeries} to an R-data.frame
#' 
#' This method extracts all events in an EventSeries, sends
#' each event's EventDate, EventType, EventValue, and state 
#' variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object.
#' 
#' @param x An object of class \code{EventSeries} 
#'        
#'
#' @return An R-data.frame containing a structured 
#'         representation of the events in the function argument
#' 
#' @seealso \code{\link{ContractType, generateEvents, processEvents}}
#'
#' @examples 
#' pam = Pam()
#' set(pam, what=list(
#'                    ContractID = "001",
#'                    Currency = "CHF",
#'                    ContractRole = "RPA",
#'                    StatusDate       = "2012-12-31T00",
#'                    ContractDealDate = "2012-12-31T00",
#'                    InitialExchangeDate = "2013-01-01T00",
#'                    MaturityDate = "2013-03-31T00",
#'                    NotionalPrincipal = 1000, 
#'                    NominalInterestRate = 0.01,
#'                    DayCountConvention = "30E/360"))
#' ad = AD0("2012-12-31T24")
#' evs = events(pam, ad)
#' class(evs)
#' 
#' evs.df = as.data.frame(evs)
#' class(evs.df)
#'
## @include
#' @export as.data.frame.EventSeries
#' @docType methods
#' @rdname as.df-methods
as.data.frame.EventSeries = function(x) {
  x$evs
}

## @include
#' @export
#' @rdname get-methods
#' @aliases get,YieldCurve,character-method
#' @aliases get,ReferenceIndex,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,ValuationEngineModel,character-method
#' @aliases get,jobjRef,character-method
setMethod( f = "get" , signature = c( "EventSeries" , "character" ) ,
           definition = function( object , what ) {
             out <- list()
             if (length(what) == 1 && tolower(what) == "all") {
               what <- FEMS:::validEVSGetFields()
             }
             for (i in what) {
               name = i
               if (is.valid.evs.get.field(i)) {
                 if (name == "id") {
                   out[[i]] <- object$id
                 } else if (name == "ct") {
                   out[[i]] <- object$ct
                 } else if (name == "evs") {
                   out[[i]] <- object$evs
                 } 
               } else {
                 warning(paste("field ", i, " does not exist, cannot get value!", sep=""))
               }
             }
             if (length(out) == 1) {
               out <- out[[1]]
             }
             return(out)
           })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ContractType,list-method
#' @aliases set,ContractType,ValuationEngine-method
#' @aliases set,ContractType,RiskFactorConnector-method
#' @aliases set,YieldCurve,list-method
#' @aliases set,ReferenceIndex,list-method
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,ValuationEngineModel,list-method
setMethod( f = "set" , signature = c( "EventSeries" , "list" ) ,
           definition = function( object , what ) {
             par.names <- names(what)
             for(i in par.names) {
               if(FEMS:::is.valid.evs.set.field(i)) {
                 value=what[[i]]
                 name=i
                 if(name == "id") {
                   object$id <- value
                 } else if(name == "ct") {
                   object$ct <- value
                 } else if(name == "evs") {
                   object$evs <- value
                 } else if(name == "EventSeries") {
                   object$evs <- value
                 }
               } else {
                 warning(paste("field ", i, " does not exist, cannot assign value!", sep=""))
               }
             }
           })

# -----------------------------------------------------------------
# Methods related to eventList
## @export
## @docType methods
## @rdname print-methods
# setMethod("print", signature = "eventList", 
#           definition = function(x, type = "pretty", indices, ...) {
#             if (type == "raw") {
#               x
#             } else {
#               y = as.data.frame(x)
#               if (!missing(indices)){ 
#                 y = y[,indices] 
#               }
#               if (type == "pretty") { 
#                 colnames(y) = .defaults$shortNames[colnames(y)]
#                 for (nam  in colnames(y))
#                 {
#                   if (is.numeric(y[,nam])) y[,nam] = round(y[,nam],4)
#                 }
#               }
#               y
#             }
#           }
# )



##############################################################
#' list-of-\code{EventSeries} to data.frame method
#'
#' Convert a list of \code{EventSeries}-elements to an R-data.frame
#' 
#' This method extracts all events in the list of EventSeries
#' objects, sends each event's EventDate, EventType, EventValue, 
#' and state variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object. An additional
#' column is added containing the \code{ContractID} of the 
#' contract from which the events have been derived.
#' 
#' @param x A \code{list} with \code{EVentSeries}-elements 
#'        
#'
#' @return An R-data.frame containing a structured 
#'         representation of the events in the function argument
#' 
#' @seealso \link{EventSeries} and \code{\link{as.data.frame.EventSeries}}
#'
#' @examples 
#' # import a portfolio
#' data(BondPortfolio)
#' ptf <- Portfolio()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' 
#' ## set analysis date
#' ad <- "2015-01-02T00"
#' 
#' # define risk factors
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(
#'   MarketObjectCode = "YC_EA_AAA",
#'   Nodes = list(ReferenceDate = ad, 
#'                Tenors = tenors, Rates = rates)))
#' cpi <- Index()
#' times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", "2018-01-01T00",
#'            "2019-01-01T00")
#' values <- c(100, 110, 120, 130, 140)
#' set(cpi, what=list(
#'   MarketObjectCode = "IND_CPI_EA",
#'   Data=list(Dates=times,Values=values)))
#' rf <- RFConn()
#' add(rf, list(yc, cpi))
#' set(ptf,rf)
#' 
#' # compute events
#' generateEvents(ptf,ad)
#' processEvents(ptf,ad)
#' 
#' # extract EventSeries from portfolio
#' evs=EventSeries(ptf)
#' class(evs)
#' 
#' # convert to data.frame
#' evs.df=as.data.frame(evs)
#' class(evs.df)
#'
## @include
#' @export
#' @docType methods
#' @rdname as.df.list-methods
as.data.frame.eventList = function (x) {
  out=do.call("rbind",lapply(x,
                             FUN = function(evs){
                               cbind(
                                 ContractID = evs$id,
                                 FEMS:::as.data.frame.EventSeries(evs))}))
  row.names(out)=NULL
  return(out)
}

##############################################################
#' \code{eventList}-print method
#'
#' Prints an \code{eventList} 
#' 
#' This method prints an object of class \code{eventList} in an formatted way.
#' 
#' Formatting options are provided
#' each event's EventDate, EventType, EventValue, and state 
#' variables from the java-object to R (through rJava) and
#' structures the data in an R-data.frame object.
#' 
#' @param x An object of class \code{eventList} 
#'        
#'
#' @return An R-data.frame containing a the structured printout
#' 
#' @seealso \code{\link{Portfolio, events}}
#'
#' @examples 
#'
## @include
#' @export
#' @docType methods
#' @rdname print-methods
setMethod("print", signature = "eventList", 
          definition = function(x, type = "pretty", indices, ...) {
            df = as.data.frame.eventList(x)
            FEMS:::print.eventList.as.data.frame(df, type=type, indices=indices, ...)
          }
)

## @include
#' @export
#' @docType methods
#' @rdname print-methods
setMethod("show", signature = "eventList", 
          definition = function(object) {
            df = as.data.frame.eventList(object)
            FEMS:::print.eventList.as.data.frame(df, type="pretty", indices=c(-5))
          }
)

## @include
#' @export
#' @docType methods
#' @rdname print-methods
setMethod("head", signature = "eventList", 
          definition = function(x, n=6, indices=c(-5), ...) {
            df = as.data.frame.eventList(x)[1:n, ]
            FEMS:::print.eventList.as.data.frame(df, type="pretty", indices=indices, ...)
          }
)

#' @export
#' @docType methods
#' @rdname print-methods
setMethod("[", signature = c("eventList", "ANY", "missing"),
          definition = function(x, i) {
            nams = names(x)
            if (is.character(i) ) {
              i = is.element(nams, i)
            }
            l = x@.Data[i]
            names(l) = nams[i]
            new("eventList", .Data=l)
          }
)




# -----------------------------------------------------------------
# private util methods

# available getter/setter fields for EventSeries
validEVSSetFields <- function() {
  return(c(
    "id", "ct", "evs", "EventSeries"
  ))
}
is.valid.evs.set.field <- function(x) {
  valid <- validEVSSetFields()
  return(x %in% valid)
}
validEVSGetFields <- function() {
  return(c(
    "id", "ct", "evs"
  ))
}
is.valid.evs.get.field <- function(x) {
  valid <- validEVSGetFields()
  return(x %in% valid)
}

getEventAttributes <- function(events, attribute){
  if (!attribute %in% c("type","time","payoff","currency",
                 "nominalValue","nominalRate","nominalAccrued")){
    stop("ErrorIn::EventSeries:: Event Attribute not known !!!")
  }
  return( unlist(lapply(events, function(x) x[[attribute]])) )
}




