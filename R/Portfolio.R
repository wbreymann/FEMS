#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that represents a portfolio of multiple rActus contracts
#' 
#' A Portfolio is a simple structure for the representation of 
#' a collection of \pkg{rActus} \code{ContractType} objects. 
#' Portfolio allows to compute cash flows and additonal analytical
#' results for the whole collection of contracts.
#' 
#' @field contracts A list for the rActus contracts the portfolio consists of 
#' 
#' @seealso \code{\link{PortfolioFast}}
#'
#' @examples
#' data(BondPortfolio)
#' ptf <- Portfolio()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' ptf
#' 
## @include
#' @export 
## @docType
#' @rdname ptf-classes
setRefClass("Portfolio",
            fields = list(
              contracts = "list",
              rf_connector = "RiskFactorConnector",
              ct_events = "data.frame"
              ))

##############################################################
#' Portfolio-class constructor
#'
#' Create an instance of \code{Portfolio} class.
#' 
#' @param ...
#'
#' @return An object of class \code{Portfolio}
#' 
#' @seealso \code{\link{PortfolioFast}}
#'
#' @examples
#' data(BondPortfolio)
#' ptf <- Portfolio()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' ptf
#'
## @include
#' @export
#' @docType methods
#' @rdname ptf-methods
## @aliases
setGeneric(name = "Portfolio",
           def = function(...){
             standardGeneric("Portfolio")
           })

## @include
#' @export
#' @rdname ptf-methods
# @aliases
setMethod(f = "Portfolio", signature = c(),
          definition = function(...){
            ptf = new("Portfolio")
            pars = list(...)
            # browser()
            if (length(pars) != 0) {
              if ("source" %in% tolower(names(pars))) {
                source = pars[["source"]]
                # browser()
                # We should try to make work the following:
                # print(
                #   import(object = ptf, source = source, pars)
                # )
                if (typeof(source)=="character") {
                  print(paste("Importing from ",source))
                  pars[["source"]] = NULL
                  data <- read.csv(source, skipNul=FALSE)
                  print("Data successfully read.")
                } else if (class(source)=="data.frame") {
                  data <- source 
                } else {
                  stop("Unknown data source.")
                }
                for (i in 1:nrow(data)) {
                  d <- data[i,]
                  # print(d)
                  ctype <- as.character(d["ContractType"])
                  # print(ctype)
                  d <- as.list(d[names(d)!="ContractType"])
                  # print(d)
                  ptf <- c(ptf, 
                           do.call(ctype, d))
                }
              } else {
                if (is.list(pars[[1]])) {
                  pars <- pars[[1]]
                }
                if ( is.null(names(pars)) ) {
                  names(pars) <- unlist(lapply(pars, function(x) get(x, "ContractID")))
                } else if (sum(names(pars)=="")>0) {
                  message("Not all contract are named. ContractID are used as names.")
                  names(pars) <- unlist(lapply(pars, function(x) get(x, "ContractID")))
                }
                if (sum("ContractABC" == unlist(lapply(pars, function(x) is(x)))) < length(pars) )
                  stop("If list is supplied, list entries must be contracts !!!")
                sapply(names(pars), function(ch) set(pars[[ch]], list("ContractID"=ch)))
                add(ptf, pars)
              }
            }
            return(ptf)
          })

## @include
## @export
setGeneric(name = "generateEvents",
           def = function(object, ...){
             standardGeneric("generateEvents")
           })


# @export
#' 
#' Sends contracts and risk factors in JSON format to server
#' (command POST from package httr)
#' 
setMethod("generateEvents", signature = c("Portfolio"),
          definition = function(object, ...){
            # send contract and risk factors to the Server

            ## create body for contracts
            contracts <- list()
            rf_conn <- RFConn()

            for (i in 1:length(object$contracts)) {
              
              ContractTerms <- object$contracts[[i]]$ContractTerms
              
              # erase NULL elements & convert dates in character formats
              contract_list <- ContractTerms[
                rapply(ContractTerms, function(x) length(grep("^NULL$",x)) == 0)]
              
              # reformat the dates to reflect java format
              contract_list <- lapply(contract_list, function(x) {
                if (is.character(x)) {
                  x_vec <- unlist(strsplit(x, ", "))
                  if (!is.na(as.Date(as.character(x_vec[1]), format = "%Y-%m-%d")) & 
                      grepl("T00$",x_vec[1])) {
                    x <- paste(paste0(x_vec,":00:00"), collapse=", ") 
                  } else if (!is.na(as.Date(as.character(x_vec[1]), format = "%Y-%m-%d")) & 
                             !grepl("T00:00:00$",x_vec[1])) {
                    x <- paste(paste0(x_vec,"T00:00:00"), collapse=", ") 
                  } else {x}
                } else {x}
              })
              
              # check if rate reset is given
              if (!is.null(contract_list$MarketObjectCodeOfRateReset)){
                temp_yc <- get(object$rf_connector,contract_list$MarketObjectCodeOfRateReset)
                if (is(temp_yc,"DynamicYieldCurve")){
                  temp_rf <- DynamicYieldCurve(
                    Rates = temp_yc$Rates,
                    DayCountConvention = temp_yc$DayCountConvention,
                    label = temp_yc$label
                  )
                } else {
                  temp_rf <- YieldCurve(
                    ReferenceDate=temp_yc$ReferenceDate, 
                    Tenors=temp_yc$Tenors, 
                    Rates=temp_yc$Rates, 
                    label = temp_yc$label,
                    DayCountConvention = temp_yc$DayCountConvention)
                }
                
                sim.data.rf(object$contracts[[i]], temp_rf)
                tst_rf <- is.rf.in.rf_conn(temp_rf, rf_conn)
                if (!tst_rf[[1]]){
                    # set the name here of this as well as in the contract_list object
                    temp_rf$label <- paste0("MarketObject_",i)
                    contract_list$MarketObjectCodeOfRateReset <- paste0("MarketObject_",i)
                    add(rf_conn, temp_rf)
                } else {
                  contract_list$MarketObjectCodeOfRateReset <- tst_rf[[2]]
                }
              }
              
              # re-format names to lower case (first letter only)
              names(contract_list) <- paste(tolower(substring(names(contract_list), 1,1)), 
                                            substring(names(contract_list), 2),sep = "")
              
              contracts[[i]] <- contract_list
            }

            # create body for risk factors
            riskFactors <- list()
            if (length(rf_conn$riskfactors) > 0) {
              for (i in 1:length(rf_conn$riskfactors)) {
                factor <- rf_conn$riskfactors[[i]]
                temp_list <- list(marketObjectCode = factor$label)
                if (is(factor, "YieldCurve")) {
                  temp_list$base <- 1
                } else {
                  temp_list$base <- factor$Data$Values[1]
                }
                temp_list$data <- data.frame(time = rownames(factor$Data), 
                                             value =  as.character(factor$Data$Values))
                temp_list$data$time <- paste0(temp_list$data$time,"T00:00:00")
                riskFactors[[i]] <- temp_list
              }
            }
            # contains both, contracts and risk factor scenarios
            fin_list <- list(contracts = contracts, 
                             riskFactors = riskFactors)

            # combine the two and create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = TRUE)
            #print(request_body)
            #browser()
            response_events <- POST(paste0(actusURL, "eventsBatch"), 
                                    body = request_body, 
                                    content_type_json())
            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(response_content$error)
              stop("ErrorIn::ContractType:: API response error; Check if all necessary ContractTerms were set correctly!!!")
            }
            return(response_content)
            
          })

##############################################################
#' Retrieve information of a \code{\link{Portfolio}}
#'
#' Allows to retrieve certain information of a portfolio. In 
#' particular, this is a convenience function to access the 
#' Reference Class's fields. Further, using \code{what='size'}
#' the number of contracts in the Portfolio is returned, for
#' \code{what='ids'} a vector of \code{ContractID} ContractTerms
#' of the contracts is returned, and if \code{what=[ContractID]}
#' with \code{[ContractID]} the ContractID of a particular 
#' contract in the portfolio then the respective contract is 
#' returned.
#' 
#' @param object An object of class \code{\link{Portfolio}}
#'        or \code{\link{PortfolioFast}}
#'        
#' @param what Either 'contracts', 'size', 'ids', or the \code{ContractID}
#'        of a particular contract in the Portfolio to be returned
#' 
#' @param ... 
#'
#' @return The respective field or other information element
#' 
#' @seealso \code{\link{set}}
#' 
#' @examples
#' # define analysis data
#' ad <- "2015-01-02T00"
#' 
#' # construct portfolio
#' data(BondPortfolio)
#' ptf <- Portfolio()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' 
#' # retrieve list of contracts
#' class(get(ptf, what="contracts"))
#' 
#' # retrieve portfolio size (number of contracts)
#' get(ptf, what="size")
#'
#' # retrieve ids of contracts in portfolio
#' get(ptf, what="ids")
#' 
#' # retrieve contract with specific id
#' get(ptf, what="110")
#'
## @include
#' @export
#' @docType methods
#' @rdname get-methods
#' @aliases get, PortfolioFast,character-method
#' @aliases get, PortfolioResults,character-method
setMethod(f = "get", signature = c("Portfolio", "character"),
          definition = function(object, what, ...){
            if ( tolower(what[1]) == "contracts" ) {
              if(is.null(object$contracts)) {
                out <- list()
              } else {
                out <- object$contracts
              }
            } else if(tolower(what[1]) == "size") {
              out <- length(object$contracts)
            } else if(tolower(what[1]) == "ids") {
              out <- unlist(lapply(object$contracts, FEMS::get,
                                   what="ContractID"))
            } else if(tolower(what[1]) == "types") {
              out <- unlist(lapply(object$contracts, FEMS::get,
                                   what="ContractType"))
            } else {
              out <- list()
              for (i in 1:length(what) ) {
                idx <- which(
                  unlist(lapply(object$contracts, FEMS::get,
                                what="ContractID"))==what[i], arr.ind=TRUE
                )
                if ( length(idx)==1 ) {
                  out[[what[i]]] <- object$contracts[[idx]]
                }
              }
            }
            ## we do not want to return a list if we only asked for one item
            if(length(out)==1) {
              out <- out[[1]]
            }
            return(out)
            
          })

##############################################################
#' Change the value of a field or other element in a
#' \code{\link{Portfolio}}
#'
#' Allows to change certain elements of a portfolio. In particular, 
#' this is a convenience function to change the 
#' Reference Class's field values. Further, the method allows to
#' assign a \code{RiskFactorConnector} to the portfolio, i.e.
#' all contracts in the portfolio.
#' See also the respective documentation in \pkg{rActus}.
#' 
#' @param object An object of class \code{\link{Portfolio}}
#'        or \code{\link{PortfolioFast}}
#'        
#' @param what Either a list with names and values of elements to change, 
#'        or an object of class \code{RiskFactorConnector} to assign 
#'        to all contracts in the portfolio.
#' 
#' @param ... 
#'
#' @return
#' 
#' @seealso \code{\link{get}}
#' 
#' @examples
#' # define analysis data
#' ad <- "2015-01-02T00"
#' 
#' # construct portfolio
#' data(BondPortfolio)
#' ptf <- Portfolio()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' 
#' # construct market model
#' yc <- YieldCurve()
#' tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
#' rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
#' set(yc, what = list(
#' MarketObjectCode = "YC_EA_AAA",
#' Nodes = list(ReferenceDate = ad, 
#' Tenors = tenors, Rates = rates)))
#' 
#' cpi <- Index()
#' times <- c("2015-01-01T00", "2016-01-01T00", "2017-01-01T00", "2018-01-01T00",
#' "2019-01-01T00")
#' values <- c(100, 110, 120, 130, 140)
#' set(cpi, what=list(
#' MarketObjectCode = "IND_CPI_EA",
#' Data=list(Dates=times,Values=values)))
#' 
#' rf <- RFConn()
#' add(rf, list(yc, cpi))
#' 
#' # assign market model to portfolio
#' set(ptf, rf, valuationEngines=TRUE)
#'
#' @include util.R
#' @export
#' @docType methods
#' @rdname set-methods
#' @aliases set,Portfolio,RiskFactorConnector-method
setMethod(f = "set",
          signature = c("Portfolio","list"),
          definition = function(object, what){
            allFields = fieldNames(object)
            setFields = names(what)
            match = match(setFields, allFields)
            matchFields = allFields[match]
            for(i in 1:length(matchFields)){
              object[[matchFields[i]]] = what[[matchFields[i]]]
            }
            
          })

## @include
#' @export
#' @docType methods
#' @rdname set-methods
#' @aliases set,Portfolio,list-method
setMethod(f = "set",
          signature = c("Portfolio","RiskFactorConnector"),
          definition = function(object, what, ...){
            pars <- list(...)
            if (!"valuationEngines" %in% names(pars)) {
              valuation <- FALSE
            } else {
              valuation <- pars[["valuationEngines"]]
            }
            
            if (!valuation) {
              for (i in object$contracts){
                set(i, what)
              }
              object$rf_connector <- what
            } else {
              for (i in object$contracts){
                stop("Method 'set' for class Portfolio must be adapted.")
                # set(i, what)
                # .jcall(.jcall(i$jref, "Lorg/actus/valuation/ValuationProvider;", "getValuationEngine"), 
                #        "V", "setRiskFactors", what$jref)
              }
            }
          })

##############################################################
#' Add contracts to the \code{\link{Portfolio}}
#'
#' Add additional contracts, i.e. \code{ContractType}-objects, 
#' to the portfolio.
#' 
#' @param object An object of class \code{\link{Portfolio}}
#'        or \code{\link{PortfolioFast}}
#'        
#' @param what Either an object of class \code{ContractType} which
#'        is added or a list of \code{ContractType}-objects in
#'        which case all elements are added to the portfolio
#' 
#' @param ... 
#'
#' @return
#' 
#' @seealso \code{\link{get}}
#' 
#' @examples
#' # load contract demo data
#' data(BondPortfolio)
#' 
#' # create new portfolio
#' ptf <- Portfolio()
#' 
#' # define subset of ContractTerms to be used to create 
#' # new PrincipalAtMaturity contract
#' # (we use a subset just to make the case here)
#' attr.names=c("ContractID", 
#'              "Currency", 
#'              "ContractRole", 
#'              "StatusDate", 
#'              "ContractDealDate",
#'              "InitialExchangeDate",
#'              "MaturityDate",
#'              "NotionalPrincipal",
#'              "NominalInterestRate",
#'              "DayCountConvention")
#' 
#' # add first contract of the demo data
#' add(ptf, Pam(as.list(BondPortfolio[1,attr.names])))
#' 
#' # add second contract of the demo data
#' add(ptf, Pam(as.list(BondPortfolio[2,attr.names])))
#'
## @include
#' @export
#' @docType methods
#' @rdname add-methods
#' @aliases add,Portfolio,list-method
#' @aliases add, PortfolioFast,ContractType-method
#' @aliases add, PortfolioFast,list-method
#' @aliases add, PortfolioResults,GranularResults-method
setMethod(f = "add", signature = c("Portfolio", "ContractType"),
          definition = function(object, what, ...){
            # ad = Analyze Date / not yet implemented
            add(object, list(what))
          })

## @include
#' @export
#' @docType methods
#' @rdname add-methods
#' @aliases add,Portfolio,list-method
#' @aliases add, PortfolioFast,ContractType-method
#' @aliases add, PortfolioFast,list-method
#' @aliases add, PortfolioResults,GranularResults-method
setMethod(f = "add", signature = c("Portfolio", "list"),
          definition = function(object, what, ...){ # ad = Analyze Date / not yet implemented
            # TODO: append slow!
            
            test <- length(object$contracts)
            object$contracts <- append(x = FEMS::get(object = object,
                                                       what = "contracts"),
                                       values = what)
            len <- length(object$contracts)
            if (!test < len) {
              cat("A problem occured")
            }
            names(object$contracts) <- CTterms(object, ,"ContractID")[,1,TRUE]
            # ids = character()
            # for (obj in what) {
            #   ids = c(ids, FEMS:::get(obj, "ContractID"))
            # }
            # names(object$contracts)[(test+1):len] = ids
          })

##############################################################
#' Remove contracts from the \code{\link{Portfolio}}
#'
#' Remove contracts from a portfolio by their respective ContractIDs.
#' 
#' @param object An object of class \code{\link{Portfolio}}
#'        or \code{\link{PortfolioFast}}
#'        
#' @param what A single or vector of characters representing the ContractIDs of the contracts to be removed
#' 
#' @param ... 
#'
#' @return
#' 
#' @seealso \code{\link{add}}
#' 
#' @examples
#' # load contract demo data
#' data(BondPortfolio)
#' 
#' # create new portfolio
#' ptf <- Portfolio()
#' 
#' # import the portfolio
#' import(ptf, BondPortfolio)
#' 
#' # get ContractIDs of the contracts in the
#' get(ptf, "ids")
#' 
#' # remove the first contract from the portfolio
#' remove(ptf, get(ptf, "ids")[1])
#'
## @include
#' @export
#' @docType methods
#' @rdname rmv-methods
setMethod(f = "remove", signature = c("Portfolio", "character"),
          definition = function(object, what, ...){ # ad = Analyze Date / not yet implemented
            # TODO: append slow!
            test = length(object$contracts)
            idx = which(get(object, "ids")%in%what)
            sapply(idx, FUN=function(x) object$contracts[[x]]=NULL)
            if(!test > length(object$contracts)){
              cat("A problem occured")
            }
          })

## @include
#' @export
# @docType methods
# @rdname add-methods
# @aliases 
setMethod(f = "summary", signature = c("Portfolio"),
          definition = function(object){
            nContr <- length(FEMS::get(object=object, what="contracts"))
            cts <- FEMS::get(object=object, what="contracts")
            if(nContr==1) {
              cts <- list(cts)
            }
            types <- lapply(X = cts, FUN = class)
            
            cat("Contains contracts:\t", nContr)
            print(table(unlist(types)))
            
            invisible(NULL)
          })

# ## @include
# #' @export
# #' @docType methods
# #' @rdname ptf-methods
## @aliases
# setGeneric(name = "show",
#            def = function(object, ...){
#              standardGeneric("show")
#            })



## @include
#' @export
# @docType methods
# @rdname show-methods
# @aliases 
setMethod(f = "show", signature = c("Portfolio"),
          definition = function(object){
            # nContr <- length(FEMS::get(object=object,what="contracts"))
            # cts <- FEMS::get(object=object, what="contracts")
            # if(nContr==1) {
            #   cts <- list(cts)
            # }
            # types <- lapply(X = cts, FUN = class)
            # 
            # cat("Contains contracts:\t", nContr)
            # print(table(unlist(types)))
            # 
            # invisible(NULL)
            print(CTterms(object, pretty))
          })

## @include
#' @export
# @docType methods
# @rdname add-methods
# @aliases 
setMethod(f = "length", signature = c("Portfolio"),
          definition = function(x){
            length(x$contracts)
          })

## @include
#' @export
#' @docType methods
#' @rdname ptf-methods
## @aliases
setGeneric(name = "CTterms",
           def = function(x, i, vars, ...){
             standardGeneric("CTterms")
           })

## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "missing", "missing"),
          definition = function(x, pretty=FALSE) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole", 
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            CTterms(x, vars=vars, pretty=pretty)
          }
)

## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "numeric", "missing"),
          definition = function(x, i, pretty=FALSE) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole",
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            # ct = x$contracts[[i]]
            CTterms(x, i, vars=vars, pretty=pretty)
          }
)

## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "logical", "missing"),
          definition = function(x, i, pretty=FALSE) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole",
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            # ct = x$contracts[[i]]
            CTterms(x, i, vars=vars, pretty=pretty)
          }
)

## @include
# #' @export
# setMethod("CTterms", signature = c("Portfolio", "character", "missing"),
#           definition = function(x, i) {
#             vars = c(
#               "ContractID",
#               "ContractType",
#               "ContractRole", 
#               "InitialExchangeDate",
#               "MaturityDate",
#               "NotionalPrincipal",
#               "NominalInterestRate"
#             )
#             # ct = x$contracts[[i]]
#             return (CTterms(x, i, vars=vars))
#           })

## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "missing", "character"),
          definition = function(x, vars, pretty=FALSE) {
            out <- extractVariablesFromPortfolio(x$contracts, vars)
            if (pretty) {
              colnames(out) <- .defaults$shortNames[colnames(out)]
            }
            return(out)
          })


## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "numeric", "character"),
          definition = function(x, i, vars, pretty=FALSE) {
            cts = x$contracts[i]
            out = data.frame()
            for (ct in cts) {
              if ( !is.null(ct) ) {
                # print(class(ct))
                # print(paste0("vars = ", vars))
                cVars = FEMS:::get(ct, vars)
                
                # print(cVars)
                # out = rbind(out, as.data.frame(FEMS:::get(ct, vars)))
                out = rbind(out, as.data.frame(cVars))
                if (pretty) {
                  colnames(out) <- .defaults$shortNames[colnames(out)]
                }
              }
            }
            out
          }
)

## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "logical", "character"),
          definition = function(x, i, vars, pretty=FALSE) {
            cts = x$contracts[i]
            out = data.frame()
            for (ct in cts) {
              if ( !is.null(ct) ) {
                # print(class(ct))
                # print(paste0("vars = ", vars))
                cVars = FEMS:::get(ct, vars)
                
                # print(cVars)
                # out = rbind(out, as.data.frame(FEMS:::get(ct, vars)))
                out = rbind(out, as.data.frame(cVars))
                if (pretty) {
                  colnames(out) <- .defaults$shortNames[colnames(out)]
                }
              }
            }
            out
          }
)


## @include
#' @export
setMethod("CTterms", signature = c("Portfolio", "character", "character"),
          definition = function(x, i, vars, pretty=FALSE) {
            # cts = x$contracts[i]
            out <- extractVariablesFromPortfolio(x$contracts[i], vars)
            if (pretty) {
              colnames(out) <- .defaults$shortNames[colnames(out)]
            }
            return(out)
          })


extractVariablesFromPortfolio = function(cts, vars) {
  out = data.frame()
  n = 0
  vl = length(vars)
  if (length(vars)==1 ) {
    for (ct in cts) {
      v = FEMS:::get(ct, vars)
      if (is.null(v)) {
        v = NA
      }
      # names(v) = vars
      df = as.data.frame(v)
      colnames(df) = vars
      # out = rbind(out, df[, vars])
      out = rbind(out, df)
    }
  } else {
    for (ct in cts) {
      v = FEMS:::get(ct, vars)
      missingNames = vars[!is.element(vars, names(v))]
      for (mN in missingNames) {
        v[[mN]] = NA
      }
      df = as.data.frame(v)
      # out = rbind(out, df[, vars])
      out = rbind(out, df)
    }
  }
  colnames(out)<-.defaults$shortNames[colnames(out)]
  return(out)
}


## @include
#' @export
setMethod("[[", signature = c("Portfolio", "ANY"),
          definition = function(x, i) {
            ct = x$contracts[[i]]
            ct
          }
)

# We use the CTterms method also for single accounts
## @include
#' @export
setMethod("CTterms", signature = c("ContractABC", "missing", "missing"),
          definition = function(x) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole", 
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            p <- Portfolio(x)
            CTterms(p, vars=vars)
          }
)


#' @export
setMethod("[[<-", signature = c("Portfolio", "ANY"),
          definition = function(x, i, value) {
            if (is.null(value)) {
              if (is.character(i)) {
                remove(x, i)
              } else {
                remove(x, ctnames(x)[i])
              }
            } else {
              set(value, list("ContractID" = i))
              add (x, value)
            }
            x
          }
)

## @include
#' @export
setMethod("[", signature = c("Portfolio", "ANY", "missing"),
          definition = function(x, i) {
            ctlist = x$contracts[i]
            ptf = Portfolio()
            add(ptf, ctlist)
            return(ptf)
          }
)

## @include
#' @export
setMethod("c", signature = c("Portfolio"),
          definition = function(x, ...) {
            pars = list(...)
            y = x$contracts
            for (p in pars) {
              if ( class(p)[1]=="Portfolio") {
                y = c(y, p$contracts)
              } else {
                y = c(y, p)
              }
            }
            ptf = Portfolio()
            ptf$contracts = y
            names(ptf$contracts) = ids(ptf)
            return(ptf)
          }
)


## @include
#' @export
#' @docType methods
#' @rdname ptf-methods
## @aliases
setGeneric(name = "ids",
           def = function(x, ...){
             standardGeneric("ids")
           })

## @include
#' @export
setMethod(f = "ids", signature = c("Portfolio"),
          definition = function(x){
            ids = character()
            ll = x$contracts
            for (i in 1:length(x)) {
              ids = c(ids,
                      FEMS:::get(ll[[i]], "ContractID")
              )
            }
            ids
          })

#' @export
setGeneric(name = "ctnames",
           def = function(object){
             standardGeneric("ctnames")
           })

#' @export
setMethod(f = "ctnames", signature = c("Portfolio"),
          definition = function(object) {
            name <- as.character(get(object, "ids"))
            return(name)
          })

#' @export
setMethod(f = "ctnames", signature = c("ContractABC"),
          definition = function(object) {
            name <- as.character(get(object, "ContractID"))
            return(name)
          })

#' @export
setGeneric(name = "ctnames<-",
           def = function(x, value){
             standardGeneric("ctnames<-")
           })

#' @export
setMethod(f = "ctnames<-", signature = c("Portfolio","ANY"),
          definition = function(x, value) {
            if (length(x) != length(value)) {
              stop('Something went wrong. Please check indices are correct !!!')
            }
            for (i in 1:length(x)){
              set(x$contracts[[i]], list("ContractID"=value[i]))
            }
            x
          })

is.rf.in.rf_conn <- function(temp_rf, rf_conn) {

  # checks if data is already included in the risk factor connector and retuns TRUE if identical, 
  # FALSE if not
  if (length(rf_conn$riskfactors)>0) {
    for (i in 1:length(rf_conn$riskfactors)){
      rfac <- rf_conn$riskfactors[[i]]
      if (identical(temp_rf$Data, rfac$Data)){
        return(list(TRUE, rfac$label))
      }
    }
  }
  return(list(FALSE,""))
}

sim.data.rf <- function(contract, rfac){
  
  # check if its a YieldCurve first... if not, skip it...
  if (is(rfac,"YieldCurve") || is(rfac,"DynamicYieldCurve")){
    anchor_dt <- contract$ContractTerms$CycleAnchorDateOfRateReset
    cycle <- contract$ContractTerms$CycleOfRateReset
    mat <- contract$ContractTerms$MaturityDate
    if (mat=="NULL"){
      # this is pretty inefficient so far, can end date be derived?
      mat <- as.character(ymd(anchor_dt) %m+% years(30))
    }
    rfac$Data <- get.data.rate.reset(rfac, anchor_dt, cycle, mat)
  }

}


##############################################################
#' Derive the events for a \code{Portfolio}
#'
#' The events of a portfolio of contracts is in fact
#' the set of events of all contracts in the portfolio.
#' For more information on events of a contract see
#' events function with \link{ContractType}-signature.
#' 
#' @param object The \code{Portfolio} or \code{PortfolioFast} for which to derive the events
#'
#' @param ad The analysis date as per which all future events are to be derived
#'
#' @param model (optional) The \code{RiskFactorConnector} conditional to which events are computed
#'  
#' @return A \code{list} object (or \code{EventTable} in case of \code{PortfolioFast} argument) containing the resulting events
#' 
#' @seealso \link{ContractType}, \link{RiskFactorConnector}, \link{EventSeries}, \link{EventTable}
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
#' 
#' # compute events
#' evs=events(ptf, ad, rf)
#' evs
#' as.data.frame(evs)
#' 
#' @include Events.R
#' @include ContractType.R
#' @include AnalysisDate.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "character", "missing"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad)))
          })

#' @include Events.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "timeDate", "missing"),
          definition = function(object, ad, model){
            return(events(object, AD0(as.character(ad))))
          })

#' @include Events.R
#' @include EventSeries.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "AD0", "missing"),
          definition = function(object, ad, model){
            out = eventList()
            i = 0
            for(x in object$contracts) {
              # i = i+1
              # print(paste("i =", i))
              tmp = events(x, ad)
              # print("After events")
              out[[as.character(tmp$id)]] = tmp
            }
            return(out)
          })


#' #' @include ContractType.R
#' #' @include AnalysisDate.R
#' #' @include EventSeries.R
#' #' @include Events.R
#' #' @export
#' #' @rdname ev-methods
#' setMethod(f = "events", signature = c("Portfolio", "AD0", "missing"),
#'           definition = function(object, ad, model){
#'             return(EventSeries(object, ad))
#'           })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include Events.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "character", "RiskFactorConnector"),
          definition = function(object, ad, model){
            return(events(object, AD0(ad), model))
          })

#' @include ContractType.R
#' @include AnalysisDate.R
#' @include RiskFactorConnector.R
#' @include EventSeries.R
#' @include Events.R
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("Portfolio", "AD0", "RiskFactorConnector"),
          definition = function(object, ad, model){
            set(object, model)
            return(EventSeries(object, ad))
          })


#' @include AnalysisDate.R
#' @include EventSeries.R
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
                Time = yearFraction(substring(time[1], 1, 10), substring(time, 1, 10), convention = "30E360"),
                NominalValue = getEventAttributes(evs_raw[[i]]$events, "nominalValue"),
                NominalRate = getEventAttributes(evs_raw[[i]]$events, "nominalRate"),
                NominalAccrued = getEventAttributes(evs_raw[[i]]$events, "nominalAccrued"))
              temp_idx <- temp_df$Type %in% c("IPCI")
              temp_df[temp_idx,"Value"] <- temp_df[temp_idx,"NominalValue"] - 
                temp_df[c(temp_idx[2:length(temp_idx)],FALSE),"NominalValue"]
              
              # need to add an AD0 event 
              idx <- length(temp_df$Date) - sum(temp_df$Date>=as.character(ad))
              if (idx == 0) {
                temp_df <- rbind(data.frame(
                  # ContractID = evs_raw[[i]]$contractId,
                  Date = as.character(ad),
                  Value = 0,
                  Type = "AD0",
                  Currency = unique(getEventAttributes(evs_raw[[i]]$events, "currency")),
                  Time = yearFraction(substring(time[1], 1, 10), as.character(ad), convention = "30E360"),
                  NominalValue = 0,
                  NominalRate = 0,
                  NominalAccrued = 0), temp_df)
              } else {
                if (any(temp_df$Date==as.character(ad))) {
                  temp_idx_df <- which(temp_df$Date==as.character(ad))
                  temp_df <- rbind(data.frame(
                    Date = as.character(ad),
                    Value = 0,
                    Type = "AD0",
                    Currency = unique(getEventAttributes(evs_raw[[i]]$events, "currency")),
                    Time = yearFraction(substring(time[1], 1, 10), as.character(ad), convention = "30E360"),
                    NominalValue = temp_df[temp_idx_df,"NominalValue"],
                    NominalRate = temp_df[temp_idx_df,"NominalRate"],
                    NominalAccrued = temp_df[temp_idx_df,"NominalAccrued"]),
                    temp_df[temp_df$Date>=as.character(ad), ])
                } else {
                  temp_df <- rbind(data.frame(
                    Date = as.character(ad),
                    Value = 0,
                    Type = "AD0",
                    Currency = unique(getEventAttributes(evs_raw[[i]]$events, "currency")),
                    Time = yearFraction(substring(time[1], 1, 10), as.character(ad), convention = "30E360"),
                    NominalValue = temp_df[idx,"NominalValue"],
                    NominalRate = temp_df[idx,"NominalRate"],
                    NominalAccrued = NaN),
                    temp_df[temp_df$Date>=as.character(ad), ])
                }
              }
              evs_list[[i]] <- temp_df
              # evs_list[[i]] <- temp_df[temp_df$Date>=as.character(ad), ]

              if (grepl("T00:00:00$",evs_raw[[i]]$contractId)) {
                #warning("ContractName contained T00:00:00. Will be removed!")
                evs_raw[[i]]$contractId <- gsub("T00:00:00","",evs_raw[[i]]$contractId)
              }
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


