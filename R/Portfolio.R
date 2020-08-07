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

            new.portfolio = new("Portfolio")
            pars = list(...)
            if ("source" %in% tolower(names(pars))) {
              source = pars[["source"]]
              pars[["source"]] = NULL
              print(
                import(object = new.portfolio, source = source, pars)
              )
            }
            return(new.portfolio)
          })


## @include
## @export
setGeneric(name = "generateEvents",
           def = function(object, ad0, ...){
             standardGeneric("generateEvents")
           })

## @include
## @export
setMethod("generateEvents", signature = c("Portfolio", "character"),
          definition = function(object, ad0){
            ad0_zdt <- FEMS::AD0(ad0)
            FEMS:::generateEvents(object, ad0_zdt)
          })

#' @include AnalysisDate.R
# @export
setMethod("generateEvents", signature = c("Portfolio", "AD0"),
          definition = function(object, ad0){
            # send contract and risk factors to the Server
            
            ## create body for contracts
            contracts <- list()
            for (i in 1:length(object$contracts)) {
              attributes <- object$contracts[[i]]$attributes
              
              # erase NULL elements & convert dates in character formats
              contract_list <- attributes[rapply(attributes, function(x) length(grep("^NULL$",x)) == 0)]
              
              # reformat the dates to reflect java format
              contract_list <- lapply(contract_list, function(x) {
                if (is.character(x)) {
                  x_vec <- unlist(strsplit(x, ", "))
                  if (!is.na(as.Date(as.character(x_vec[1]), format = "%Y-%m-%d")) & grepl("T00$",x_vec[1])) {
                    x <- paste(paste0(x_vec,":00:00"), collapse=", ") 
                  } else if (!is.na(as.Date(as.character(x_vec[1]), format = "%Y-%m-%d")) & !grepl("T00:00:00$",x_vec[1])) {
                    x <- paste(paste0(x_vec,"T00:00:00"), collapse=", ") 
                  } else {x}
                } else {x}
              })
              
              # re-format names to lower case (first letter only)
              names(contract_list) <- paste(tolower(substring(names(contract_list), 1,1)), 
                                            substring(names(contract_list), 2),sep = "")
              contracts[[i]] <- contract_list
            }
            
            # create body for risk factors
            riskFactors <- list()
            if (length(object$rf_connector$riskfactors) > 0) {
              for (i in 1:length(object$rf_connector$riskfactors)) {
                factor <- object$rf_connector$riskfactors[[i]]
                temp_list <- list(marketObjectCode = factor$MarketObjectCode)
                if (is(factor, "YieldCurve")) {
                  temp_list$base <- 1
                } else {
                  temp_list$base <- factor$Data$Values[1]
                }
                temp_list$data <- data.frame(time = factor$Data$Dates, 
                                             value =  as.character(factor$Data$Values))
                temp_list$data$time <- paste0(temp_list$data$time,"T00:00:00")
                riskFactors[[i]] <- temp_list
              }
            }
            fin_list <- list(contracts = contracts, 
                             riskFactors = riskFactors)
            
            # combine the two and create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = TRUE)
            response_events <- POST(paste0(ActusURL, "eventsBatch"), 
                                    body = request_body, 
                                    content_type_json())
            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(response_content$error)
              stop("ErrorIn::ContractType:: API response error; Check if all necessary attributes were set correctly!!!")
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
#' \code{what='ids'} a vector of \code{ContractID} attributes
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
                set(i, what)
                .jcall(.jcall(i$jref, "Lorg/actus/valuation/ValuationProvider;", "getValuationEngine"), 
                       "V", "setRiskFactors", what$jref)
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
#' # define subset of attributes to be used to create 
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
            nContr <- length(FEMS::get(object=object,what="contracts"))
            cts <- FEMS::get(object=object, what="contracts")
            if(nContr==1) {
              cts <- list(cts)
            }
            types <- lapply(X = cts, FUN = class)
            
            cat("Contains contracts:\t", nContr)
            print(table(unlist(types)))
            
            invisible(NULL)
          })

## @include
#' @export
# @docType methods
# @rdname add-methods
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
            print(CTvars(object))
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
setGeneric(name = "CTvars",
           def = function(x, i, vars, ...){
             standardGeneric("CTvars")
           })

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "numeric", "missing"),
          definition = function(x, i) {
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
            CTvars(x, i, vars=vars)
          }
)

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "numeric", "character"),
          definition = function(x, i, vars) {
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
              }
            }
            out
          }
)

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "character", "missing"),
          definition = function(x, i) {
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
            return (CTvars(x, i, vars=vars))
          })

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "character", "character"),
          definition = function(x, i, vars) {
            # cts = x$contracts[i]
            return(extractVariablesFromPortfolio(x$contracts[i], vars))
          })


## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "missing", "character"),
          definition = function(x, vars) {
            return(extractVariablesFromPortfolio(x$contracts, vars))
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
  return(out)
}

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "missing", "missing"),
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
            CTvars(x, vars=vars)
          }
)


## @include
#' @export
setMethod("[[", signature = c("Portfolio", "ANY"),
          definition = function(x, i) {
            ct = x$contracts[[i]]
            ct
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
