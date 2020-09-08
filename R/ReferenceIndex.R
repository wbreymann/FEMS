#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class extending \code{\link{RiskFactor}} class
#' and representing a reference index risk factor
#' 
#' Reference indices define a class of market risk factors 
#' that in the ACTUS model may directly affect future cash 
#' flows arising from a financial instrument, e.g. Inflation-
#' linked bonds, or are used for valuation of instruments, 
#' e.g. a Stock market index when using CAPM.
#' 
#' @field MarketObjectCode character name of the risk factor 
#' @field TimeSeries data.frame representating time series data
#' 
#' @seealso \code{\link{RiskFactor, YieldCurve, ForeignExchangeRate}}
#'
#' @examples
#' 
#' # create an ReferenceIndex object
#' ind <- Index()
#' 
#' # define time stamps and values
#' times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#'            "2019-01-01")
#' values <- c(100, 110, 120, 130, 140)
#' 
#' # set the MarketObjectCode and Data
#' set(ind, what = list(MarketObjectCode = "CHF_SMI",
#'                      Data = list(Dates = times, 
#'                                        Values = values)))
#' 
#' # get MarketObjectCode
#' get(ind, "MarketObjectCode")
#' 
#' # get values of the risk factor at certain times
#' valueAt(ind, "2016-01-01")
#' valueAt(ind, c("2016-01-01", "2018-07-01", "2018-07-01"))
#' 
#' @include RiskFactor.R
#' @export
#' @rdname ind-classes
setRefClass("ReferenceIndex", contains = "RiskFactor",
            fields = list())

##############################################################
#' \code{ReferenceIndex}-class constructor
#'
#' Create an instance of \code{ReferenceIndex} class. The 
#' constructor will also create an instance of the respective
#' Java class in the running JVM.
#' 
#' @param ...
#'
#' @return An object of class \code{ReferenceIndex} 
#'          containing the reference to the Java object
#' 
#' @seealso \code{\link{YieldCurve, ForeignExchangeRate}}
#'
## @include
#' @export
#' @docType methods
#' @rdname ind-methods
#' @aliases Index-method
setGeneric(name = "Index",
           def = function(data, charvec, label, ...){
             standardGeneric("Index")
           })

## @include
#' @export
#' @rdname ind-methods
## @aliases 
setMethod(f = "Index",signature = c("numeric", "character", "character"),
          definition = function(data, charvec, label, ...){
            object <- new("ReferenceIndex")
            object$Data <- timeSeries(data = data, 
                                      charvec = charvec, 
                                      units = "Values")
            object$label <- label
            return(object)
          })

## @include
#' @export
#' @rdname set-methods
#' @aliases set,ForeignExchangeRate,list-method
#' @aliases set,YieldCurve,list-method
setMethod(f = "set", signature = c("ReferenceIndex", "list"),
          definition = function(object, what, ...){
            par.names <- names(what)
            for (i in par.names) {
              if (FEMS:::is.valid.index.set.field(i)) {
                value <- what[[i]]
                switch(i,
                       label = {
                         object$label = value
                       },
                       Data = {
                         object$Data = timeSeries(data = as.numeric(value$Values), 
                                                  charvec = as.character(value$Dates),
                                                  units = "Values")
                       } )
              } else {
                warning(paste("field ", i, " does not exist, cannot assign value!", sep=""))
              }
            }
          })

## @include
#' @export
#' @rdname get-methods
#' @aliases get,RiskFactorConnector,character-method
#' @aliases get,ForeignExchangeRate,character-method
#' @aliases get,YieldCurve,character-method
#get(ind, "MarketObjectCode")
setMethod(f = "get", signature = c("ReferenceIndex", "character"),
          definition = function(object, what, ...){
            out <- list()
            if (length(what) == 1 && tolower(what) == "all") {
              what <- FEMS:::validIndexGetFields()
            }
            for (i in what) {
              if (is.valid.index.get.field(i)) {
                out[[i]] <- switch(i,
                                   label = {
                                     object$label
                                     },
                                   Dates = {
                                     rownames(idx$Data)
                                     },
                                   Values = {
                                     object$Data$Values
                                     },
                                   Data = object$Data
                )
              } else {
                warning(paste("field ", i, " does not exist, cannot get value!", sep = ""))
              }
            }
            if (length(out) == 1) {
              out <- out[[1]]
            }
            return(out)
          })

#' @include ForeignExchangeRate.R
#' @export
#' @rdname vat-methods
#' @aliases vat-method
#' @aliases vat,ReferenceIndex,character-method
setMethod(f = "valueAt", signature = c("ReferenceIndex", "character"),
          definition = function(object, at, ...){
            datums <- sort(as.Date(unlist(rownames(object$Data))))
            bool_matrix <- t(sapply(at, function(x) datums <= as.Date(x)))
            indices <- unname(apply(bool_matrix,1,function(x) max(which(x))))
            return(object$Data[,"Values"][indices])
          })

## @include
#' @export
setMethod(f = "show", signature = c("ReferenceIndex"),
          definition = function(object){
            cat(paste0("Label: ", object$label,"\n"))
            print("Time Series:")
            print(object$Data)
          })


## -----------------------------------------------------------------
## helper methods
# existing fields in the java class
validIndexSetFields <- function() {
  return(c(
    "Data", "label"
  ))
}
is.valid.index.set.field <- function(x) {
  valid <- validIndexSetFields()
  return(x %in% valid)
}
validIndexGetFields <- function() {
  return(c(
    "label", "Dates", "Values", "Data" 
  ))
}
is.valid.index.get.field <- function(x) {
  valid <- validIndexGetFields()
  return(x %in% valid)
}

##################################################
#testing
##################################################
# at=c("2016-01-01", "2018-07-01", "2018-07-01")
# timeSeries=data.frame(Dates= as.character(times),Values=as.numeric(values))
# timeSeries["2015-01-01"]
# timeSeries[1,2]
#
# timeSeries["Dates"]=="2016-01-01"
# timeSeries["Dates"].==at
#
# "2016-01-01" %in% timeSeries["Dates"]
# "2016-01-01"==timeSeries["Dates"]
#
# match(at,timeSeries["Dates"])
#
# times=c("2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01")
# at=c("2015-01-01","2018-01-01")
# intersect(at, times)
#
#
# 1:10 %in% c(1,3,5,9)
# sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","%")
# sstr[sstr %in% c(letters, LETTERS)]
#
# "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
# (1:10) %w/o% c(3,7,12)
# ## Note that setdiff() is very similar and typically makes more sense:
#         c(1:6,7:2) %w/o% c(3,7,12)  # -> keeps duplicates
# setdiff(c(1:6,7:2),      c(3,7,12)) # -> unique values
#
# ## Illuminating example about NA matching
# r <- c(1, NA, NaN)
# zN <- c(complex(real = NA , imaginary =  r ), complex(real =  r , imaginary = NA ),
#         complex(real =  r , imaginary = NaN), complex(real = NaN, imaginary =  r ))
# zM <- cbind(Re=Re(zN), Im=Im(zN), match = match(zN, zN))
# rownames(zM) <- format(zN)
# zM ##--> many "NA's" (= 1) and the four non-NA's (3 different ones, at 7,9,10)
#
# length(zN) # 12
# unique(zN) # the "NA" and the 3 different non-NA NaN's
# stopifnot(identical(unique(zN), zN[c(1, 7,9,10)]))
#
# ## very strict equality would have 4 duplicates (of 12):
# symnum(outer(zN, zN, Vectorize(identical,c("x","y")),
#                      FALSE,FALSE,FALSE,FALSE))
# ## removing "(very strictly) duplicates",
# i <- c(5,8,11,12)  # we get 8 pairwise non-identicals :
# Ixy <- outer(zN[-i], zN[-i], Vectorize(identical,c("x","y")),
#                      FALSE,FALSE,FALSE,FALSE)
# x <- sample(1:10)
# match(at,times)
#
# at==times
#

#times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#           "2019-01-01")
#values <- c(100, 110, 120, 130, 140)
#timeSeries=data.frame(Dates= as.character(times),Values=as.numeric(values),stringsAsFactors=FALSE)
#
#timeSeries=data.frame(Dates= times,Values=values)
#a=timeSeries$Dates[1,1]
#timeSeries[2,1]

# ind <- Index()
# times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#            "2019-01-01")
# values <- c(100, 110, 120, 130, 140)
# set(ind, what=list(MarketObjectCode = "CHF_SMI",
#     TimeSeries=list(Dates=times,Values=values)))
# get(ind, "MarketObjectCode")
# valueAt(ind, "2016-01-01")
# valueAt(ind, c("2016-01-01", "2018-01-01", "2018-01-01"))
# 
# atTS <- c("2016-01-01", "2018-01-01", "2018-01-01")
# match(times,atTS)
# match(atTS,times)
# times %in% atTS

# ind <- Index()
# times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
#            "2019-01-01")
# values <- c(100, 110, 120, 130, 140)
# set(ind, what=list(MarketObjectCode = "CHF_SMI",
#                    TimeSeries=list(Dates=times,Values=values)))
# get(ind, "MarketObjectCode")
# valueAt(ind, "2016-01-01")
# valueAt(ind, c("2016-01-01", "2018-07-01", "2018-07-01"))