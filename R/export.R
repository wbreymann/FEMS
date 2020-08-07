#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' export the contract events and further results of a portfolio
#'
#' This method allows to export the ACTUS contract events and
#' further results such as discount factors (if computed) or
#' selection criteria (if added) for a portfolio to a flat-file.
#' 
#' @param object The portfolio whose events and further results to export. 
#'        Can be an object of class \code{\link{Portfolio}} or 
#'        \code{\link{PortfolioFast}}.
#' 
#' @param file A character containing the file path with name to
#'        the target file
#'        
#' @param sep A character containing the separator to be used when
#'        writing the file
#' 
#' @return
#' 
#' @seealso \code{\link{Portfolio}},\code{\link{PortfolioFast}},\code{\link{import}}
#'
#' @examples
#' # define analysis data
#' ad <- "2015-01-02T00"
#' 
#' # construct portfolio
#' data(BondPortfolio)
#' ptf <- PortfolioFast()
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
#' # generate and process events
#' generateEvents(ptf, ad0=ad)
#' processEvents(ptf, ad0=ad)
#' 
#' # export events to external file
#' export(ptf, file="./temp.txt", sep=";")
#' # ... and import again to check results structure
#' res <- read.table("./temp.txt", sep=";", header=TRUE)
#' str(res)
#'
# @include
#' @export
#' @docType methods
#' @rdname exprt-methods
#' @aliases import,Portfolio,character,character-method
#' @aliases import,PortfolioFast,character,character-method
setGeneric(name = "export",
           def = function(object, file, sep){
             standardGeneric("export")
           })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname exprt-methods
#' @aliases import,PortfolioFast,character,character-method
setMethod("export",
          signature = c("Portfolio"),
          definition = function(object, file="./events.txt", sep=","){
            out = FEMS::as.data.frame(FEMS::EventSeries(object$contracts[[1]]))
            out = cbind(FEMS::get(object$contracts[[1]], 
                                    c("ContractID", "ContractType", 
                                      "LegalEntityIDRecordCreator", 
                                      "LegalEntityIDCounterparty")
            ), out)
            for(i in 1:length(object$contracts)) 
              out = rbind(out,
                          cbind(FEMS::get(object$contracts[[i]],
                                            c("ContractID", "ContractType", 
                                              "LegalEntityIDRecordCreator", 
                                              "LegalEntityIDCounterparty")
                          ),
                          FEMS::as.data.frame(FEMS::EventSeries(object$contracts[[i]]))
                          )
              )
            write.table(out, file=file, sep=sep, col.names=TRUE, row.names=FALSE)
          })