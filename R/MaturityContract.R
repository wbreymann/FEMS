#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
#' A Reference Class that extends \code{"ContractType"} 
#' representing all Maturity Contracts
#' 
#' Class \link{MaturityContract} is superclass to all maturity
#' Contract Types in the ACTUS taxonomy (cf. 
#' \link{www.projectactus.org}).
#' 
#' @seealso \code{\link{ContractType, PrincipalAtMaturity, Annuity}}
#'
## @examples
#' 
#' @include ContractType.R
#' @export 
#' @rdname ct-classes
setRefClass("MaturityContract",
            contains = "ContractType",
            fields = list(
            ))

## -----------------------------------------------------------------
## internal (private) function returning all contract types that 
## are maturity types
maturityContractTypes <- function() {
  c("pam", "principalatmaturity",
    "ann", "annuity",
    "nam", "negativeamortizer",
    "lam", "linearamortizer",
    "lax", "exoticlinearamortizer"
  )
}