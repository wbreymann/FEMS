% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EventSeries.R
\docType{methods}
\name{print,EventSeries-method}
\alias{print,EventSeries-method}
\alias{[,eventList,ANY,missing-method}
\title{\code{EventSeries}-print method}
\usage{
\S4method{print}{EventSeries}(x, type = "pretty", indices, round.value = 0, ...)

\S4method{[}{eventList,ANY,missing}(x, i)
}
\arguments{
\item{x}{An object of class \code{EventSeries}}
}
\value{
An R-data.frame containing a the structured printout
}
\description{
Prints an \code{EventSeries}
}
\details{
This method prints an object of class \code{EventSeries} in an formatted way.

Formatting options are provided
each event's EventDate, EventType, EventValue, and state 
variables from the java-object to R (through rJava) and
structures the data in an R-data.frame object.
}
\examples{
pam = Pam()
set(pam, what=list(
                   ContractID = "001",
                   Currency = "CHF",
                   ContractRole = "RPA",
                   StatusDate       = "2012-12-31T00",
                   ContractDealDate = "2012-12-31T00",
                   InitialExchangeDate = "2013-01-01T00",
                   MaturityDate = "2013-03-31T00",
                   NotionalPrincipal = 1000, 
                   NominalInterestRate = 0.01,
                   DayCountConvention = "30E/360"))
ad = AD0("2012-12-31T24")
evs = events(pam, ad)
class(evs)

print(evs)
print(evs, indices=c(-4,-9))

}
\seealso{
\code{\link{ContractType, events}}
}
