% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loan.R
\name{loan}
\alias{loan}
\title{\code{loan}}
\usage{
loan(start, maturity, nominal, ir, irFreq, role, amortFreq, amort)
}
\arguments{
\item{start}{a character reflecting the start date of the loan.}

\item{maturity}{a character reflecting the maturity of the loan, 
default is "0 years".}

\item{nominal}{a numeric to set the notional principal of the loan, 
default is 0.0.}

\item{ir}{a numeric to set the nominal interest rate, default is 0.0.}

\item{irFreq}{a character reflecting the frequency of interest rate payments, 
default is "1 year".}

\item{role}{a character reflecting the contract role. default is "long".}

\item{amortFreq}{a character reflecting the frequency of principal redemption 
payments, default is "1 year".}

\item{amort}{a numeric to set the amount of principal redemption.}
}
\value{
a LinearAmortizer contrat with specified attributes.
}
\description{
Constructor method for simple LinearAmortizer contracts.
}
\examples{
l <- loan("2013-12-31", maturity = "5 years", nominal = 50000, 
           ir = 0.02, irFreq = "1 years", amortFreq = "1 years")
           
}
