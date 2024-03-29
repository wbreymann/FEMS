% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ann.R
\name{annuity}
\alias{annuity}
\title{\code{annuity}}
\usage{
annuity(start, nominal, ir, annuity, annuityFreq, maturity, role)
}
\arguments{
\item{start}{a character reflecting a start date of the annuity.}

\item{nominal}{a numeric to set the notional principal of the annuity, 
default is 0.0.}

\item{ir}{a numeric to set the nominal interest rate, default is 0.0.}

\item{annuity}{a numeric to set the next principal redemption payment.}

\item{annuityFreq}{a character reflecting the frequency of redemption payments, 
default is "1 year".}

\item{maturity}{a character reflecting the maturity of the annuity, 
default is "0 years".}

\item{role}{a character reflecting the contract role, default is "long".}
}
\value{
an Annuity contract with specified attributes.
}
\description{
Constructor method for simple Annuity contracts. If interest payments are set, the 
cycle of interest rate payments is the same as the cycle for annuity payments.
}
\examples{
ann <- annuity("2013-12-31", nominal = 50000, ir = 0.02, maturity = "5 years")

}
