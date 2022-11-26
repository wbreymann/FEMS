#*************************************************************
# Copyright (c) 2018 - present by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************



##############################################################
#' Simulate new (future) business on the PortfolioTree
#' 
#' A going-concern view on an organization (or \link{PortfolioTree}) includes
#' the simulation of continuation of the organization's business. This method
#' allows to compute new future business in terms of ACTUS \link{ContractTypes}
#' according to a certain model contract and target growth on each "leaf" 
#' (or terminal account) in the \link{PortfolioTree}.
#' 
#' New business is generated along the time axis defined by method parameter 
#' \code{by}. At each point in time in \code{by}, the target and actual 
#' quantity in business, as measured in terms of nominal value, are compared and for the positive 
#' parts of difference \code{target - actual} new business is generated in form 
#' of new \link{ContractType}. The new business is then added to the \link{Portfolio}
#' of existing contracts as provided with method argument \code{ptf}.
#' 
#' The new business generating process is defined by the following information 
#' elements provided with the \link{PortfolioTree} in method argument \code{tree}:
#' \itemize{
#' \item{leafs}{The leafs, or terminal accounts, 
#' provide the structure for the 
#' simulation of new business. That is, scenarios for new business can be 
#' defined only for the leafs of a \link{PortfolioTree}.}
#' \item{business}{Business contains a model 
#' contract in terms of a list of
#' named ACTUS Contract Attributes (as would be used in argument \code{what} 
#' used with the \link{set}-method applied to a \link{ContractType}). This
#' model contract is used as a template for all new business contracts 
#' simulated on this particular leaf.}
#' \item{targets}{A \code{(number of leafs) x (number of leafs)} 
#' numeric matrix
#' containing on the diagonal the growth rates (\code{by}-period wise) on the 
#' leafs themselves and on the outer-diagonals the spill-over rates from one
#' leaf to another leaf.}
#' }
#' 
#' For a demo use \code{demo(NewBusinessSimulation)}.
#' 
#' @param ptf A \link{Portfolio} of \link{ContractTypes} which gives the existing
#' business.
#' 
#' @param tree A \link{PortfolioTree} with fields \code{leafs}, \code{business}, 
#' \code{targets} specified that drives the new business generating process.
#' 
#' @param by A vector of ordered \link{timeDate}s providing the time-axis along 
#' which the business simulation is to be conducted.
#' 
#' @param rf The \link{RiskFactorConnector} providing the market environment under 
#' which the business scenario is to be evaluated.
#' 
#' @return
#' 
#' @seealso \link{PortfolioTree}
#' 
#' @examples
#' 
#' @include ModelStructure.R
#' @export
#' @rdname newbiz-method
setGeneric(name = "newbiz",
              def = function(object, by, growth, templates, rf){
                standardGeneric("newbiz")
              })
# setGeneric(name = "newbiz",
#            def = function(ptf,tree,by,growth,templates,rf){
#              standardGeneric("newbiz")
#            })

## @include 
#' @export
#' @rdname newbiz-method
setMethod(f = "newbiz",
          signature = c("Node", "timeDate", "matrix", 
                        "list", "RiskFactorConnector"),
          definition = function(object, by, growth, templates, rf) {

            # extract nodes
            nodes <- Traverse(object, filterFun = isLeaf)
            # nodes <- nodes[!(Get(nodes,"name") %in% c("Equity","Operations","Current"))]
            #nodes <- Traverse(object, traversal = "pre-order") # or rather this???
            # The whole path should be taken into account
            tmp <- unlist(lapply(nodes, function(x) x$name))
            # print ("tmp:")
            # print(tmp)
            nodes <- nodes[
              !sapply(tmp, 
                      FUN=function(x) sum(is.element(x, c("Equity","Revenues","Expenses", "Current"))) )
            ]
            
            # test inputs
            if( !all.equal(
              length(nodes), nrow(growth), ncol(growth), length(templates)) ) {
              stop(paste0("Dimensions mismatch: number of nodes and dimensions of", 
                          "(square-) growth-matrix and templates-vector are not the same!"))
            }
            
            # compute current volume
            # events(object, by[1], rf, end_date = as.character(by[length(by)])
            
            by.helper <- timeSequence(by[1], by = "1 year", length.out=2)
            tb <- timeBuckets (by.helper, bucketLabs = substr(as.character(by.helper),1,4))
            value.node <- value(object, by = tb, type = "nominal")[,1, drop=FALSE]
            
            # current value
            current <- abs(value.node[sapply(names(Get(nodes, "levelName")), 
                                             grep, x=rownames(value.node)),])
            names(current) <- names(Get(nodes, "levelName"))
            
            # compute new business targets
            growth <- diag(diag(growth)[names(current)])
            targets <- sapply(2:length(by), function(x) current %*% growth^(x-1))
            rownames(targets) <- names(current)
            
            # prepare list of new business portfolios
            # newbizseq = list()
            newbizseq <- make.copy(object, empty=TRUE)
            # browser()
            # sequentially compute new business (contracts) along simulation timeline
            for(i in 2:length(by)) {
              
              # initialize empty new business portfolio
              newbiz <- make.copy(object, empty=TRUE)
              
              # new current business
              by.helper <- timeSequence(by[i], by = "1 year", length.out=2)
              tb <- timeBuckets (by.helper, bucketLabs = substr(as.character(by.helper),1,4))
              new.model <- add.model(object,newbizseq)
              events(new.model, as.character(by[i]), rf, end_date = as.character(by[length(by)]))
              value.node <- value(new.model, by=tb, type="nominal")[,1, drop=FALSE]
              current <- abs(value.node[sapply(names(Get(nodes, "levelName")), 
                                      grep, x=rownames(value.node)),])
              
              #current=abs(unlist(lapply(nodes,function(x) value(events,by=as.character(by),type="nominal",filter=list(x)))))
              
              # compute new volume
              fillAmounts <- targets[,i-1] - current
              fillAmounts[is.na(fillAmounts)] <- 0
              
              # compute new business (of template type) by leaf
              for(leaf in names(fillAmounts[fillAmounts!=0])) {
                
                # instantiate new business template for this leaf
                leaf.model.temp <- templates[[leaf]]
                leaf.model.terms <- leaf.model.temp$ContractTerms
                leaf.model.terms <- leaf.model.terms[rapply(leaf.model.terms, function(x) length(grep("^NULL$",x)) == 0)]
                leaf.model <- CT(longName(leaf.model.terms$ContractType))
                set(leaf.model, leaf.model.terms)
                
                
                
                # set id indicating the leaf and time-increment
                leaf.model$ContractTerms$ContractID <- paste(by[i], leaf, sep=".")
                # set notional to current new volume
                leaf.model$ContractTerms$NotionalPrincipal <- as.numeric(fillAmounts[leaf])
                # adjust maturity date (if set at all)
                if(leaf.model$ContractTerms$MaturityDate!="NULL") {
                  # Compute new maturity
                  leaf.model$ContractTerms$MaturityDate = 
                    format(as.POSIXct(by[i]) + 
                             round(as.POSIXct(leaf.model$ContractTerms$MaturityDate,format="%Y-%m-%d") -
                                     as.POSIXct(leaf.model$ContractTerms$InitialExchangeDate,format="%Y-%m-%d"), 
                                   0), "%Y-%m-%d")
                }
                # adjust premium discount if defined
                # if( length(leaf.model$ContractTerms$FixPremiumDiscount)>0 && 
                #     leaf.model$ContractTerms$FixPremiumDiscount ) {
                #   rate.pd = rates(
                #     get(rf, leaf.model$ContractTerms$MarketObjectOfCodePremiumDiscount), 
                #     termEnd = leaf.model$ContractTerms$RateTerm, 
                #     termStart = paste0(by[i],"T00")
                #   ) + leaf.model$ContractTerms$RateSpread
                #   leaf.model$ContractTerms$PremiumDiscountAtIED = fillAmounts[leaf] * rate.pd
                # }
                # adjust interest rate if defined
                
                if( leaf.model$ContractTerms$CycleOfRateReset != "NULL") {
                  leaf.model$ContractTerms$NominalInterestRate = as.character(rates(
                    get(rf, leaf.model$ContractTerms$MarketObjectCodeOfRateReset),
                    from = as.character(by[i]),
                    by = substr(leaf.model$ContractTerms$CycleOfRateReset,2,3),
                  ) + as.numeric(leaf.model$ContractTerms$RateSpread))
                }
                # set initial exchange date to current date
                leaf.model$ContractTerms$InitialExchangeDate <- as.character(by[i]-24*3600)
                
                # remove non-ACTUS terms from template in order to prevent from warning messages
                # leaf.model$ContractTerms$FixInterestRate = NULL
                # leaf.model$ContractTerms$FixPremiumDiscount = NULL
                # leaf.model$ContractTerms$MarketObjectCodePremiumDiscount = NULL
                
                # add contract to new business portfolio
                addContracts(list(leaf.model), FindNode(newbiz, leaf))
              }
              
              # add risk factor connector for execution of contracts
              # set(newbiz, rf)
              
              # add portfolio to new business sequence
              # newbizseq[[as.character(by[i])]]=newbiz
              newbizseq <- add.model(newbizseq, newbiz)
              #names(newbizseq$contracts) = ids(newbizseq)
            }
            
            # return new business sequence
            return(newbizseq)
            })
