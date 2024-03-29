% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bankAccount.R
\name{bankAccount}
\alias{bankAccount}
\title{\code{bankAccount}}
\usage{
bankAccount(
  start,
  balance = 0,
  accrued = 0,
  ir = 0,
  irFreq = "1 year",
  ext_transactions = NULL,
  int_transfers = NULL,
  perc_outflows = NULL,
  currency = "CHF",
  variable.rates = FALSE,
  ...
)
}
\arguments{
\item{start}{Start date of the account}

\item{balance}{Initial balance of the account}

\item{accrued}{Accrued interest up to the start date}

\item{ir}{Interest rate}

\item{irFreq}{Interest rate frequency}

\item{ext_transactions}{timeSeries object indicating ExternalTransactions}

\item{int_transfers}{timeSeries object indicating InternalTransfers}

\item{perc_outflows}{timeSeries object indicating PercentageOutflows}

\item{currency}{Currency of the account}

\item{variable.rates}{Logical, if the account has variable rates.}
}
\value{
An object of class \code{CurrentAccount}
}
\description{
Function which creates an instance of \code{CurrentAccount}.
}
\examples{
cashflows <- timeSeries(c(1000,1000,30000,10000,-20000), units="CHF", 
                timeSequence(from="2014-04-01", by="year", length.out=5))
my.account <- bankAccount("2013-12-31", balance=50000, 
                         ext_transactions = cashflows, ir=0.02)

}
