% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\docType{methods}
\name{import}
\alias{import}
\alias{import,Portfolio,data.frame-method}
\alias{import,Portfolio,character-method}
\title{import portfolios of contracts from a source of contract data}
\usage{
import(object, source, ...)

\S4method{import}{Portfolio,data.frame}(object, source, ...)

\S4method{import}{Portfolio,character}(object, source, ...)
}
\arguments{
\item{object}{The portfolio where to append the imported contracts. 
Can be an object of class \code{\link{Portfolio}} or 
\code{\link{PortfolioFast}}.}

\item{source}{The data source where ACTUS contract data is imported
from. This can be a \code{data.frame} or a character string 
with the full filename of the file from which the data is read.}

\item{...}{Additional parameters can be a \code{character} 
'sheet' giving the name of the sheet to import when 
loading from Excel, \code{character} 'sep' specifying
the separator when loading from text-file, or boolean
'valuationEngines' for all data sources.}
}
\value{
a \code{data.frame} containing all the exceptions which were thrown
}
\description{
This method allows to import ACTUS contracts from a data 
source and append the contracts to an R-portfolio.
}
\details{
Parameter \code{object} contains the portfolio to which
to append the imported contracts. Note that existing 
contracts in the portfolio are not removed. Yet, in case
of \code{object} being a \code{\link{PortfolioFast}}, 
existing contracts with same 'ContractID' than newly
imported contracts are overwritten.

The data source must contain ACTUS contract data in a table 
format with column names indicating the ACTUS contract 
attribute using official attribute long-names 
(cf. \url{www.projectactus.org}).
Currently, three data formats are supported: 
\itemize{
   \item R-data.frame
   \item MS Excel 'xls' files
   \item flat-files such as 'txt' or 'csv'
}
If the data is in flat-file format, parameter \code{source} 
contains the path to the file. Further, additional parameters
'sheet' or 'sep' may be used.

The contract data can be extended with additional parameters
specifying a valuation engine and its parameters for every
contract. The format for such parameters is as follows:
\itemize{
   \item 'Valuation_[engine]'
   \item 'Valuation_[par1]'
   \item 'Valuation_[par2]' 
   \item ...
}
where '[engine]' specifies the type of valuation engine 
(currently supported are 'Discounting', 'MultiCurrencyDiscounting',
'CapitalAssetPricingModel') and '[par1]', ... refer to parameters
to the specific type of valuation engine:
\itemize{
   \item Discounting: 'RiskFactorObjectLink', 'dc.spread'
   \item MultiCurrencyDiscounting: 'CurrencyPair', 'TargetCurrency',
   'Currency1', 'Currency2', 'Spread1', 'Spread2', 
   'InterestRateModelObjectLink1', 'InterestRateModelObjectLink2'
   \item CapitalAssetPricingModel: 'IndexObjectLink', 'RiskFreeRatesObjectLink',
   'MarketValueObserved', 'ModelAlpha', 'ModelBeta', 'StatusDate'
}

Further, any number of selection criteria for aggregation purposes 
can be added using the following format: 'Selection_[criteria]' 
where '[criteria]' refers to the specific criteria e.g. 'LineOfBusiness'.
Notice that the selection criteria are added to the results table
of a \code{\link{PortfolioFast}} only.
}
\examples{
# define analysis data
ad <- "2015-01-02T00"

# load demo portfolio data
data(BondPortfolio)

# check data structure
# (note the format of certain column headers, 
# e.g. Valuation_XX, or Selection_XX)
str(BondPortfolio)

# now create portfolio structure and import demo data
# as portfolio
ptf <- PortfolioFast()
import(ptf,BondPortfolio, valuationEngines=TRUE)

# check portfolio
ptf
get(ptf, what="ids")

}
\seealso{
\code{\link{Portfolio}},\code{\link{PortfolioFast}},\code{\link{export}}
}
