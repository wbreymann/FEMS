% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ForeignExchangeRate.R, R/ReferenceIndex.R
\docType{methods}
\name{valueAt}
\alias{valueAt}
\alias{valueAt,ForeignExchangeRate,character-method}
\alias{vat-method}
\alias{vat,ReferenceIndex,character-method}
\alias{valueAt,ReferenceIndex,character-method}
\title{Generic method to retrieve the value of a 
\code{\link{ForeignExchangeRate,ReferenceIndex}} object at 
a certain point in time}
\usage{
valueAt(object, at, ...)

\S4method{valueAt}{ForeignExchangeRate,character}(object, at, ...)

\S4method{valueAt}{ReferenceIndex,character}(object, at, ...)
}
\arguments{
\item{object}{An object of class \code{RiskFactor} for 
which to return it's value at a given point in time}

\item{at}{The point in time at which to return the value}

\item{...}{}
}
\value{
numeric The value of the respective risk factor
       at the specified point in time
}
\description{
\code{\link{ForeignExchangeRate,ReferenceIndex}} risk
factors define the dynamics of the respective risk factor 
over time. \code{valueAt} computes or retrieves the value 
of this risk factor at a future point in time.
}
\examples{

# create an FX-Rate object
fx <- FxRate() 

# define time stamps and values
times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
           "2018-01-01", "2019-01-01")
values <- c(1.04, 1.05, 1.2, 1.0, 0.9)

# set the MarketObjectCode and TimeSeries
set(fx, what = list(MarketObjectCode = "CHF/USD",
                    TimeSeries = list(Dates=times,Values=values)))  

# get the MarketObjectCode
get(fx, "MarketObjectCode")

# get values of the risk factor at certain times
valueAt(fx, "2016-01-01")
valueAt(fx, c("2016-01-01", "2018-07-01", "2018-07-01"))

}
