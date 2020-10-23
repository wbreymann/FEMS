
library(FEMS)


rm(list=ls())
devtools::load_all()

# library(R6)

pp <- institution("PowerPlant")

pp
pp$Assets$Current$contracts[[1]]$InternalTransfers <- timeSeries()
pp$Assets$Current$contracts[[1]]$InternalTransfers 


class(pp)[1]
tail(class(pp),1)



