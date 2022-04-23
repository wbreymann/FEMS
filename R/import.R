#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' import portfolios of contracts from a source of contract data
#'
#' This method allows to import ACTUS contracts from a data 
#' source and append the contracts to an R-portfolio.
#' 
#' Parameter \code{object} contains the portfolio to which
#' to append the imported contracts. Note that existing 
#' contracts in the portfolio are not removed. Yet, in case
#' of \code{object} being a \code{\link{PortfolioFast}}, 
#' existing contracts with same 'ContractID' than newly
#' imported contracts are overwritten.
#' 
#' The data source must contain ACTUS contract data in a table 
#' format with column names indicating the ACTUS contract 
#' attribute using official attribute long-names 
#' (cf. \url{www.projectactus.org}).
#' Currently, three data formats are supported: 
#' \itemize{
#'    \item R-data.frame
#'    \item MS Excel 'xls' files
#'    \item flat-files such as 'txt' or 'csv'
#' }
#' If the data is in flat-file format, parameter \code{source} 
#' contains the path to the file. Further, additional parameters
#' 'sheet' or 'sep' may be used.
#' 
#' The contract data can be extended with additional parameters
#' specifying a valuation engine and its parameters for every
#' contract. The format for such parameters is as follows:
#' \itemize{
#'    \item 'Valuation_[engine]'
#'    \item 'Valuation_[par1]'
#'    \item 'Valuation_[par2]' 
#'    \item ...
#' }
#' where '[engine]' specifies the type of valuation engine 
#' (currently supported are 'Discounting', 'MultiCurrencyDiscounting',
#' 'CapitalAssetPricingModel') and '[par1]', ... refer to parameters
#' to the specific type of valuation engine:
#' \itemize{
#'    \item Discounting: 'RiskFactorObjectLink', 'dc.spread'
#'    \item MultiCurrencyDiscounting: 'CurrencyPair', 'TargetCurrency',
#'    'Currency1', 'Currency2', 'Spread1', 'Spread2', 
#'    'InterestRateModelObjectLink1', 'InterestRateModelObjectLink2'
#'    \item CapitalAssetPricingModel: 'IndexObjectLink', 'RiskFreeRatesObjectLink',
#'    'MarketValueObserved', 'ModelAlpha', 'ModelBeta', 'StatusDate'
#' }
#' 
#' Further, any number of selection criteria for aggregation purposes 
#' can be added using the following format: 'Selection_[criteria]' 
#' where '[criteria]' refers to the specific criteria e.g. 'LineOfBusiness'.
#' Notice that the selection criteria are added to the results table
#' of a \code{\link{PortfolioFast}} only.
#' 
#' 
#' @param object The portfolio where to append the imported contracts. 
#'        Can be an object of class \code{\link{Portfolio}} or 
#'        \code{\link{PortfolioFast}}.
#' 
#' @param source The data source where ACTUS contract data is imported
#'        from. This can be a \code{data.frame} or a character string 
#'        with the full filename of the file from which the data is read.
#' 
#' @param ... Additional parameters can be a \code{character} 
#'        'sheet' giving the name of the sheet to import when 
#'        loading from Excel, \code{character} 'sep' specifying
#'        the separator when loading from text-file, or boolean
#'        'valuationEngines' for all data sources.
#' 
#' 
#' @return a \code{data.frame} containing all the exceptions which were thrown
#' 
#' @seealso \code{\link{Portfolio}},\code{\link{PortfolioFast}},\code{\link{export}}
#'
#' @examples
#' # define analysis data
#' ad <- "2015-01-02T00"
#' 
#' # load demo portfolio data
#' data(BondPortfolio)
#' 
#' # check data structure
#' # (note the format of certain column headers, 
#' # e.g. Valuation_XX, or Selection_XX)
#' str(BondPortfolio)
#' 
#' # now create portfolio structure and import demo data
#' # as portfolio
#' ptf <- PortfolioFast()
#' import(ptf,BondPortfolio, valuationEngines=TRUE)
#' 
#' # check portfolio
#' ptf
#' get(ptf, what="ids")
#'
## @include
#' @export
#' @docType methods
#' @rdname imprt-methods
#' @aliases import,Portfolio,data.frame-method
#' @aliases import,Portfolio,character-method
setGeneric(name = "import",
           def = function(object, source, ...){
             standardGeneric("import")
           })
#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname imprt-methods
#' @aliases import,Portfolio,character-method
setMethod(f = "import", signature = c("Portfolio", "data.frame"),
          definition = function(object, source, ...) {

            temp.file <- tempfile(pattern = "", fileext = ".csv")
            temp.sep <- ";"
            write.table(source, file=temp.file, sep=temp.sep, row.names=FALSE,
                        quote=FALSE)
            silent <- import(object, temp.file, sep=temp.sep, ...)
            ## out message
            ex <- paste(FEMS:::get(object, "size"), " CTs imported from data.frame based on ", temp.file, sep = "")
            return(ex)
          })

#' @include Portfolio.R
#' @export
#' @docType methods
#' @rdname imprt-methods
#' @aliases import,Portfolio,data.frame-method

setMethod(f = "import", signature = c("Portfolio", "character"),
          definition = function(object, source, ...) {

            pars <- list(...)
            splitfile <- strsplit(source, ".", fixed=TRUE)[[1]]
            extension <- splitfile[length(splitfile)]
            
            if (extension == "xls") {
              port_data  <- loadFromExcel(source, pars)
              ex <- "loaded from Excel file"
            } else if(extension%in%c("txt", "csv")) {
              port_data <- loadFromText(source, pars)
              ex <- "loaded from flat file"
            } else {
              ex <- "file extension not recognized! supported formats are '.xls', '.txt', '.csv'"
            }
            
            port_data[is.na(port_data)] <- "NULL"
            #port_data <- port_data[, -grep("_", colnames(port_data))]
            #port_data <- port_data[,-which(names(port_data) %in% "X")]
            
            ## get loaded IDs
            ctIDs <- port_data$ContractType
            
            ## add contracts to the portfolio
            for (i in 1:length(ctIDs)) {

              long_name <- longName(tolower(ctIDs[i]))
              contract <- CT(long_name)
              ContractTerms <- as.list(t(port_data[i,]))
              names(ContractTerms) <- colnames(port_data)
              
              # also drop everything that is not part of the model...
              contract_model <- CTM(long_name)
              idx_notvalid <- !(names(ContractTerms) %in% names(contract_model$allowed))
              ContractTerms <- ContractTerms[!idx_notvalid]
              
              # drop all NULL elements...
              ContractTerms <- ContractTerms[rapply(ContractTerms, function(x) length(grep("^NULL$",x)) == 0)]
              
              set(object = contract, what = ContractTerms)
              add(object, contract)
            }
            
            ## out message
            ex <- paste(length(ctIDs), " CTs ", ex, sep = "")
            return(ex)
          })

# an internal util function
loadFromExcel <- function(source, pars) {
  ## check parameters
  if (!"sheet" %in% names(pars)) {
    stop("please specify parameter 'sheet', the name of the sheet from which to read!")
  } else {
    sheet <- pars[["sheet"]]
  }
  
  if (!"valuationEngines" %in% names(pars)) {
    valuation <- FALSE
  } else {
    valuation <- pars[["valuationEngines"]]
  }
  
  port_data <- data.frame(read_excel(path = source, sheet = pars$sheet, na = "NA"))
  return(port_data)
}

# an internal util-function
loadFromText <- function(source, pars) {
  ## check parameters
  if(!"sep" %in% names(pars)) {
    seperator <- ","
  } else {
    seperator <- pars[["sep"]]
  }
  
  if(!"valuationEngines"%in%names(pars)) {
    valuation <- FALSE
  } else {
    valuation <- pars[["valuationEngines"]]
  }
  
  portfolio_df=read.csv(source,header=TRUE,sep = seperator,
                        colClasses=c(DayCountConvention="character"))
  # colClasses=c(DayCountConvention="character") should be set 
  # because otherwise the value that is interpreted as an integer and set to Inf
  # #replace NULL with NA
  # # data_cleaned <- apply(csv_sample,2,function(x) sub("NULL",NA, x))
  # # data_cleaned[1,!is.na(data_cleaned[1,])]
  # # data_cleaned$ContractType
  # #replace NA with NULL
  # data_cleaned = csv_sample
  # data_cleaned[is.na(data_cleaned)]="NULL"
  # 
  # a=data_cleaned[1,]
  # a$ContractType
  # contract_name=longName(a$ContractType)
  # CT(contract_name)
  # 
  # 
  # a=csv_sample[1,]
  # is.null(a)
  # b=a[!is.na(a)]
  # 
  # ## java helper object
  # browser()
  # jhelper <- .jnew("org.rfl.portfolio.dataio.FlatFileLoaderHelper")
  # 
  # ## load data
  # .jcall(jhelper, "V", "load", source, sep, valuation)
  return(portfolio_df)
}


############################################################################
#Experimenting
# ptf= Portfolio()
# object=ptf
# source="./data/BondPortfolio.xls"
# import(ptf, source=source,sheet="BondPortfolio", sep=",", valuationEngines=TRUE)

# source="./data/BondPortfolio_sample.csv"
# import(ptf, source=source,sheet="BondPortfolio", sep=",", valuationEngines=TRUE)



# port_data1=port_data
# write.csv(port_data,"./data/BondPortfolio_sample.csv", row.names = F)
# 
# #csv_sample2=csv_sample[,1:]
# View()
# dim(csv_sample)
 
# port_data <- read.table(file = "../../BondPortfolio.xls", sep = "", header = TRUE)
# port_data <- read.table(file = "../../BondPortfolio.xls")
# 
# library("readxl")
# my_data <- read_excel("../../BondPortfolio.xls",na="NA")
# my_data[is.na(my_data)]="NULL"

