#' @include ContractABC.R
#' @export
#' @rdname ct-classes
setRefClass("FEMSContract",
            contains = "ContractABC",
            fields = list())

## -----------------------------------------------------------------
#' @export
setMethod(f = "get", signature = "FEMSContract",
          function(object, what, ...){
            if(what[[1]]=="all") what=FEMS:::terms(object)
            fields <- sapply(what,function(x) {
              if (class(try(object$field(x), silent=TRUE))=="try-error") {
                "N/A"
              } else {
                object$field(x)
              }
            })
            return(as.list(fields))
          })