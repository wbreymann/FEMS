
library(FEMS)


rm(list=ls())
devtools::load_all()

# library(R6)

pp <- institution("PowerPlant")

pp
pp$Assets$Current$contracts

class(pp)[1]
tail(class(pp),1)

