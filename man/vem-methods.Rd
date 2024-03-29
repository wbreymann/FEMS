% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValuationEngine.R, R/DiscountingEngine.R,
%   R/CapmEngine.R, R/MultiCurrencyDiscountingEngine.R
\docType{methods}
\name{VEM}
\alias{VEM}
\alias{VEM,jobjRef-method}
\alias{VEM,character-method}
\alias{DcModel}
\alias{CapmModel}
\alias{CapmModel,ANY-method}
\alias{XfxDcModel}
\alias{XfxDcModel,ANY-method}
\title{\code{ValuationEngineModel}-class constructor}
\usage{
VEM(object)

DcModel(...)

CapmModel(...)

\S4method{CapmModel}{ANY}(...)

XfxDcModel(...)

\S4method{XfxDcModel}{ANY}(...)
}
\arguments{
\item{object}{If an object of class \code{jobjRef}, then 
this parameter is expected to be a reference to a Java
discounting engine model and a new R-object of the same 
class is created where the Java reference is attached 
as the classes' \code{jref}-field. If a character, then 
\code{object} is expected to be the R-class name of the 
valuation engine for which a valuation engine model is
to be instantiated.}
}
\value{
An object of a class extending \code{DiscountingEngineModel}
}
\description{
Create an instance of an implementation of class 
\code{ValuationEngineModel} (e.g. 
\code{\link{DiscountingEngineModel}}, etc).
}
\details{
This constructor is in fact a short cut to the constructors
of the implemented classes such as \code{\link{DcModel}} for 
\code{\link{DiscountingEngineModel}}. Note 
that it is not possible to instanciate class 
\code{DiscountingEngineModel} itself but only the implementing 
classes extending class \code{DiscountingEngineModel}.
}
\examples{
# example 1: create a new 'DiscountingEngine' object
dcm = VEM("DiscountingEngine")

# example 2: attach the reference to a Java 'DiscountingEngineModel'
#            object to a new R-'DiscountingEngineModel' object. Note,
#            the new object will refer to the same Java model.
dcm = VEM("DiscountingEngine")
set(dcm, what=list(
                   RiskFactorObjectLink="YC_CHF",
                   dc.spread=0.05))
# same.dcm = VEM(dcm$jref)   # This command doesn't work.

# example 3: create a new 'DiscountingEngineModel' object using the
#            Engine-specific model constructor method
dcm = DcModel()

}
\seealso{
\code{\link{ValuationEngineModel,DiscountingEngine}}
}
