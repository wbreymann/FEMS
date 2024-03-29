% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/duration.R
\name{duration}
\alias{duration}
\title{\code{duration}}
\usage{
duration(x, type="macaulay", yield, price, isPercentage=TRUE, from)
}
\arguments{
\item{x}{the contract or portfolio of contracts for which to calculate 
the duration.}

\item{type}{a \code{character} defining the type of duration; possible types are 
'fisher-weil', 'macaulay' (default) and 'dollar'.}

\item{yield}{a \code{numeric}, an Object of class \code{YieldCurve} or 
\code{DynamicYieldCurve} that describes the spot rate term structure
or indicates the yield used to calculate the duration.}

\item{price}{a \code{numeric}, indicating the price used for calculating the yield to maturity
of each contract.}

\item{isPercentage}{a \code{numeric}, indicating if the 'yield' is passed as percentage 
(TRUE) or as fraction (FALSE).}

\item{from}{a \code{character} indicating the date as for which the net present value
is calculated.}
}
\value{
a \code{numeic} containing the calculated duration.
}
\description{
Function which calculates the duration of a portfolio or a contract.
}
\details{
For the Macaulay duration, if \code{yield} is not provided, \code{price} should
be provided and is used to calculate the  \code{yield}.
For the Fisher-Weil duration, \code{yieldCurve} must be specified. In this
case the argument \code{price} has no effect.
}
\examples{
bnd1=bond(start="2015-01-01", maturity="30 years", nominal=1000, 
          coupon=0.06, couponFreq="1 year", role="long", variable=FALSE)
duration(bnd1, type="macaulay", yield=9)

}
