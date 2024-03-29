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
#' @import data.tree R6
#' @export
#' @rdname institution
Institution <- R6Class("Institution",
                       inherit = Node)

#' @export
#' @rdname institution
institution <- function(name, cashcollect=TRUE, equity=TRUE, Operations=TRUE, ...) {
  
  inst <- Node$new(name)
  inst$AddChild("Assets")
  inst$AddChild("Liabilities")
  inst$Liabilities$AddChild("Debt")
  if (equity) { inst$Liabilities$AddChild("Equity") }
  if (Operations) { 
    inst$AddChild("Operations") 
    inst$Operations$AddChild("Revenues") 
    inst$Operations$AddChild("Expenses") 
  }
  
  if (cashcollect) {
    inst$Assets$AddChild("Current")
    collector <- CurrentAccount(ContractID = "Collector", 
                                CycleOfInterestPayment = "1Y-",
                                CycleOfRateReset = "1Y-",
                                ...)
    addContracts(list(collector=collector), 
                 FindNode(inst, "Current"))
    inst$Assets$AddChild("ShortTerm")
    inst$Assets$AddChild("LongTerm")
  }

  return(inst)
}


# Comment
# 'ModelStructure.R' doesn't contain any class definition.
# Maybe the methods in that file should be integrated into this one,
# but this can wait.

# events method:
# the method defined in 'ModelStructure.R' could be just extended such that 
# all cashflow-relevant events are added to the cash collector as internal transfers.
# But then, as last step, the event function should be executed again 
# for the cash collector and the eventList of the corresponding node must be
# updated accordingly, cf. lines 114-119 of 'ModelStructure'
# 
