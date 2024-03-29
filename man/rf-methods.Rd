% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RiskFactor.R
\docType{methods}
\name{RF}
\alias{RF}
\alias{RF,character-method}
\alias{as.timeSeries}
\title{\code{RiskFactor}-class constructor}
\usage{
RF(object)

\S4method{RF}{character}(object)

as.timeSeries(x)
}
\arguments{
\item{object}{character, where \code{object} is expected to be the 
R-class name of the RiskFactor-implementation to be instantiated.}
}
\value{
An object of a class extending \code{RiskFactor}
}
\description{
Create an instance of an implementation of class 
\code{RiskFactor} (e.g. \code{\link{YieldCurve}},
\code{\link{ReferenceIndex}}, etc). 
This constructor is in fact a short cut to the constructors
of the implemented classes such as \code{\link{YieldCurve}} for 
class \code{\link{YieldCurve}}, \code{\link{Index}} for 
class \code{\link{ReferenceIndex}} or \code{\link{FxRate}} for 
class \code{\link{ForeignExchangeRate}}. Note that it is not possible to
instanciate class \code{RiskFactor} itself but only the
implementing classes extending \code{RiskFactor}.
}
\examples{
# create a new 'YieldCurve' object
yc = RF("YieldCurve")

}
\seealso{
\code{\link{YieldCurve, ReferenceIndex}}
}
