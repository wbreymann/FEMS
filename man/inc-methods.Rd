% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Income.R, R/ModelStructure.R, R/Operations.R
\docType{methods}
\name{income}
\alias{income}
\alias{income,ContractType,timeDate,missing,missing,missing-method}
\alias{income,ContractType,timeBuckets,missing,missing,missing-method}
\alias{income,ContractType,timeDate,character,missing,missing-method}
\alias{income,ContractType,timeBuckets,character,missing,missing-method}
\alias{income,ContractType,timeDate,missing,logical,missing-method}
\alias{income,ContractType,timeBuckets,missing,logical,missing-method}
\alias{income,ContractType,timeDate,character,logical,missing-method}
\alias{income,ContractType,timeBuckets,character,logical,missing-method}
\alias{income,ContractType,timeDate,missing,logical,ValuationEngine-method}
\alias{income,ContractType,timeBuckets,missing,logical,ValuationEngine-method}
\alias{income,ContractType,timeDate,character,logical,ValuationEngine-method}
\alias{income,ContractType,timeBuckets,character,logical,ValuationEngine-method}
\alias{income,Portfolio,timeDate,missing,missing,missing-method}
\alias{income,Portfolio,timeBuckets,missing,missing,missing-method}
\alias{income,Portfolio,timeDate,missing,missing,ValuationEngine-method}
\alias{income,Portfolio,timeBuckets,missing,missing,ValuationEngine-method}
\alias{income,Portfolio,timeDate,character,logical,missing-method}
\alias{income,Portfolio,timeBuckets,character,logical,missing-method}
\alias{income,Portfolio,timeDate,character,logical,ValuationEngine-method}
\alias{income,Portfolio,timeBuckets,character,logical,ValuationEngine-method}
\alias{income,EventSeries,timeDate,missing,missing,missing-method}
\alias{income,EventSeries,timeBuckets,missing,missing,missing-method}
\alias{income,EventSeries,timeDate,character,missing,missing-method}
\alias{income,EventSeries,timeBuckets,character,missing,missing-method}
\alias{income,EventSeries,timeDate,missing,logical,missing-method}
\alias{income,EventSeries,timeBuckets,missing,logical,missing-method}
\alias{income,EventSeries,timeDate,character,logical,missing-method}
\alias{income,EventSeries,timeBuckets,character,logical,missing-method}
\alias{income,EventSeries,timeDate,missing,logical,ValuationEngine-method}
\alias{income,EventSeries,timeBuckets,missing,logical,ValuationEngine-method}
\alias{income,EventSeries,timeDate,character,logical,ValuationEngine-method}
\alias{income,EventSeries,timeBuckets,character,logical,ValuationEngine-method}
\alias{income,eventList,timeDate,missing,ANY,ANY-method}
\alias{income,eventList,timeDate,character,ANY,ANY-method}
\alias{income,eventList,timeDate,character,logical,missing-method}
\alias{income,data.frame,timeDate,character,ANY,ANY-method}
\alias{income,data.frame,timeDate,character,logical,missing-method}
\alias{income,Node,timeBuckets,ANY,ANY,ANY-method}
\alias{income,Operations,timeDate,missing,ANY,ANY-method}
\alias{income,Operations,timeBuckets,missing,ANY,ANY-method}
\alias{income,Operations,timeBuckets,character,ANY,ANY-method}
\title{Derive the income-vector for \code{ContractType}}
\usage{
income(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,character,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,character,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,missing,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,missing,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,missing,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,missing,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeDate,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{ContractType,timeBuckets,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeDate,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeBuckets,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeDate,missing,missing,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeBuckets,missing,missing,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeDate,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeBuckets,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeDate,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Portfolio,timeBuckets,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,missing,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,character,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,character,missing,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,missing,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,missing,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,missing,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,missing,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeDate,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{EventSeries,timeBuckets,character,logical,ValuationEngine}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{eventList,timeDate,missing,ANY,ANY}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{eventList,timeDate,character,ANY,ANY}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{eventList,timeDate,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{data.frame,timeDate,character,ANY,ANY}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{data.frame,timeDate,character,logical,missing}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Node,timeBuckets,ANY,ANY,ANY}(object, by, type, revaluation.gains, method, scale = 1, digits = 2)

\S4method{income}{Operations,timeDate,missing,ANY,ANY}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Operations,timeBuckets,missing,ANY,ANY}(object, by, type, revaluation.gains, method, ...)

\S4method{income}{Operations,timeBuckets,character,ANY,ANY}(object, by, type, revaluation.gains, method, ...)
}
\arguments{
\item{object}{The \code{ContractType} or \code{EventSeries}-object for which to derive the income-vector}

\item{by}{A sequence of 'timeDate's providing the target time-axis for the income-vector}

\item{type}{A character representing the type of income (either 'marginal' or 'cumulative')}

\item{method}{A 'ValuationEngine' (or list thereof) giving the valuation methods used when computing the re-valuation gains as part of income}

\item{...}{(optional) Use parameter 'revaluation.gains=FALSE' in order to return income solely from 
interest/fee payments}
}
\value{
A \code{numeric} object representing the income-vector on the target time-axis
}
\description{
Income of a contract is computed over a (number of) specific time
interval(s) defined by argument \code{by}. Within such a time interval,
income is composed of two components: (1) nominal income as the
net payments from interest and fees, and (2) income from re-valuation
of the contract at beginning and end of the time interval. If at the 
begining of the time interval the contract's 'mark-to-model" value is
higher than at the end a re-valuation gain results and vice versa.
Thereby, the valuation model may be defined for each contract prior
to calling this function or specified using the function parameter 
'method'.
}
\details{
Different income-concepts can be derived for a financial instrument
or the resulting EventSeries, respectively. Currentently, these are
Marginal income and Cumulative income.

Marginal income-vector represents the aggregate income from interest
and fee payments within a
set of user-defined time-intervals. The time-intervals are defined as
a sequence of timeDate-dates. Thereby, the marginal income-vector
gives the net interest+fee cash flows within the specified 
time-intervals.

Cumulative income-vector is the cumulative sum over time (-intervals)
of the marginal income-vector.
}
\examples{
pam <- Pam()
set(pam, what=list(
                 ContractID = "001",
                 Currency = "CHF",
                 Calendar = "Weekday",
                 ContractRole = "RPA",               
                 StatusDate       = "2016-05-30T00",
                 ContractDealDate = "2016-05-30T00",
                 InitialExchangeDate = "2016-05-30T00",
                 MaturityDate = "2020-06-01T00",
                 NotionalPrincipal = 1000,
                 NominalInterestRate = 0.05,
                 CycleOfInterestPayment = "1Y-", 
                 PremiumDiscountAtIED = 0.0,
                 DayCountConvention = "30E/360",
                 BusinessDayConvention = "SCF"))
ad <- "2016-06-01T00"

# generate event series
evs=events(pam, ad)

# define target income time axis
by=timeSequence(substring(ad, 1, 10), "2020-06-01", by="1 year")

# derive marginal income from interest and fee payments for defined time axis
income(pam, by, "marginal", revaluation.gains=FALSE)

# derive cumulative income
income(pam, by, "cumulative", revaluation.gains=FALSE)

# now include revaluation gains
# therefore, define market environment and valuation method
yc=YieldCurve()
set(yc, what=list(Nodes=list(ReferenceDate=ad,
                             Tenors=c("1M", "10Y"),
                             Rates=c(0.005, 0.02)),
                  MarketObjectCode = "RiskFreeCurve"))
rf=RFConn()
add(rf, yc)
dcEngine <- DcEngine()
set(dcEngine, list(RiskFactorObjectLink="RiskFreeCurve",
                  dc.spread=0.0))
set(dcEngine, rf)

# now compute income with revaluation gains for defined time axis
income(pam, by, "marginal", dcEngine, revaluation.gains=TRUE)
income(pam, by, "cumulative", dcEngine, revaluation.gains=TRUE)

}
\seealso{
\code{\link{ContractType}} and \code{\link{EventSeries}}
}
