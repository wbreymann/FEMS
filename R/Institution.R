#*******************************************************************************
# ZHAW Risk and Finance Lab
# package: rflContracts
# Date: 14.09.2015
# IDP - Institute for Data Analysis and Process Design
#*******************************************************************************

##############################################################
#' 
#' Class that contains the whole model of an enterprise or institution.
#' 
#' The class inherits from class \code{Node} in package \code{data.tree}
#' It contains a hierarchical model structure.
#' The upper level is predefined with the nodes "Active", "Passive" and
#' "Operations".
#' 
#' @import data.tree, R6
#' @export
#' @rdname institution
#' @export
Institution <- R6Class("Institution",
                       inherit = Node)


#' @export
#' @rdname institution
#' @export
institution <- function(name, cashcollect=TRUE) {
  inst <- Node$new("inst")
  inst$AddChild("Assets")
  inst$AddChild("Liabilities")
  inst$AddChild("PandL")
  inst$Assets$AddChild("Current")
  
  collector <- CurrentAccount()
  addContracts(list(collector=collector), FindNode(inst, "Current"))
  return(inst)
}
