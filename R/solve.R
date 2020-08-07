#' @export 
setGeneric(name = "solve",
           def = function(method, ...){
             standardGeneric("solve")
           })

#' @export 
setMethod(f = "solve", signature = c("character"),
          definition = function(method, rate=NaN, period=NaN, capital_0=NaN, capital_t=NaN, ...) {
            # convert to list
            inputs <- list(rate=rate, period=period, capital_0=capital_0, capital_t=capital_t)
            
            # check for only one variable to be NaN
            isnan <- names(inputs[is.na(inputs)])
            if (length(isnan) != 1){
              stop("Exactly one variable is allowed to be empty!")
            }
            out <- solve.for.exponential(isnan, inputs)
            return(out)
            })

solve.for.exponential <- function(name, inputs){
  
  # solve for formula capital_t=capital_0*(1+rate)^period
  if (name == "rate") {
    out <- (inputs$capital_t/inputs$capital_0)^(1/inputs$period)-1
  } else if (name == "period") {
    out <- log(inputs$capital_t/inputs$capital_0)/log(1+inputs$rate)
  } else if (name == "capital_0") {
    out <- inputs$capital_t/((1+inputs$rate)^inputs$period)
  } else if (name == "capital_t") {
    out <- inputs$capital_0*(1+inputs$rate)^inputs$period
  } else {
    stop("Name not allowed")
  }
  return(out)
}


solve2 = function(object, method, ...) {
  eval(call(method, f=object, unlist(list(...))))
}

# Simple example:
solve2(object=function(x) x^2-1, method="uniroot", lower=-2, upper=0)
solve2(object=function(x) x^2-1, method="uniroot", lower=0, upper=2)

