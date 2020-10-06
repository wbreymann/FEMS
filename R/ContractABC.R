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

