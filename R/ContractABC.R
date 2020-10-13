##############################################################
#' A Reference Class parent to all ACTUS & FEMS Contract Types
#' 
#' This class is only used in an abstract sense in that it is
#' not intended to create instances of it. It's use is to 
#' serve as parent to various implementations of ACTUS and FEMS CTs 
#' as means for designing method-inheritage. 
#' 
#' @export
setRefClass("ContractABC",
            fields = list())

#' @export
setGeneric(name = "ctnames",
           def = function(object){
             standardGeneric("ctnames")
           })

#' @export
setMethod(f = "ctnames", signature = c("ContractABC"),
          definition = function(object) {
            name <- as.character(get(object, "ContractID"))
            return(name)
          })


# And here we construct a simple "show" method for accounts
#' @export
setMethod(f = "show", signature = c("ContractABC"),
          definition = function(object){
            print(CTterms(object))
          })

# ... and a summary method identical to the show method.
# This should be differentiated.
#' @export
setMethod(f = "summary", signature = c("ContractABC"),
          definition = function(object){
            print(CTterms(object))
          })
