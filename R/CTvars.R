## @include
#' @export
#' @docType methods
#' @rdname ptf-methods
## @aliases
setGeneric(name = "CTvars",
           def = function(x, i, vars, ...){
             standardGeneric("CTvars")
           })

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "numeric", "missing"),
          definition = function(x, i) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole", 
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            # ct = x$contracts[[i]]
            CTvars(x, i, vars=vars)
          }
)

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "numeric", "character"),
          definition = function(x, i, vars) {
            cts = x$contracts[i]
            out = data.frame()
            for (ct in cts) {
              if ( !is.null(ct) ) {
                # print(class(ct))
                # print(paste0("vars = ", vars))
                cVars = FEMS:::get(ct, vars)
                # print(cVars)
                # out = rbind(out, as.data.frame(FEMS:::get(ct, vars)))
                out = rbind(out, as.data.frame(cVars))
              }
            }
            out
          }
)

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "character", "missing"),
          definition = function(x, i) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole", 
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            # ct = x$contracts[[i]]
            return (CTvars(x, i, vars=vars))
          })

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "character", "character"),
          definition = function(x, i, vars) {
            # cts = x$contracts[i]
            return(FEMS:::extractVariablesFromPortfolio(x$contracts[i], vars))
          })


## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "missing", "character"),
          definition = function(x, vars) {
            return(FEMS:::extractVariablesFromPortfolio(x$contracts, vars))
          })

extractVariablesFromPortfolio = function(cts, vars) {
  out = data.frame()
  n = 0
  vl = length(vars)
  if (length(vars)==1 ) {
    for (ct in cts) {
      v = FEMS:::get(ct, vars)
      if (is.null(v)) {
        v = NA
      }
      # names(v) = vars
      df = as.data.frame(v)
      colnames(df) = vars
      # out = rbind(out, df[, vars])
      out = rbind(out, df)
    }
  } else {
    for (ct in cts) {
      v = FEMS:::get(ct, vars)
      missingNames = vars[!is.element(vars, names(v))]
      for (mN in missingNames) {
        v[[mN]] = NA
      }
      df = as.data.frame(v)
      # out = rbind(out, df[, vars])
      out = rbind(out, df)
    }
  }
  return(out)
}

## @include
#' @export
setMethod("CTvars", signature = c("Portfolio", "missing", "missing"),
          definition = function(x) {
            vars = c(
              "ContractID",
              "ContractType",
              "ContractRole", 
              "InitialExchangeDate",
              "MaturityDate",
              "NotionalPrincipal",
              "NominalInterestRate"
            )
            CTvars(x, vars=vars)
          }
)


## @include
#' @export
setMethod("[[", signature = c("Portfolio", "ANY"),
          definition = function(x, i) {
            ct = x$contracts[[i]]
            ct
          }
)

## @include
#' @export
setMethod("[", signature = c("Portfolio", "ANY", "missing"),
          definition = function(x, i) {
            ctlist = x$contracts[i]
            ptf = Portfolio()
            add(ptf, ctlist)
            return(ptf)
          }
)

## @include
#' @export
setMethod("c", signature = c("Portfolio"),
          definition = function(x, ...) {
            pars = list(...)
            y = x$contracts
            for (p in pars) {
              if ( class(p)[1]=="Portfolio") {
                y = c(y, p$contracts)
              } else {
                y = c(y, p)
              }
            }
            ptf = Portfolio()
            ptf$contracts = y
            names(ptf$contracts) = ids(ptf)
            return(ptf)
          }
)


## @include
#' @export
#' @docType methods
#' @rdname ptf-methods
## @aliases
setGeneric(name = "ids",
           def = function(x, ...){
             standardGeneric("ids")
           })

## @include
#' @export
setMethod(f = "ids", signature = c("Portfolio"),
          definition = function(x){
            ids = character()
            ll = x$contracts
            for (i in 1:length(x)) {
              ids = c(ids,
                      FEMS:::get(ll[[i]], "ContractID")
              )
            }
            ids
          })

