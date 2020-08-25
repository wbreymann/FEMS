# change csv files used from previous actus core attributes

rm(list=ls())

data <- read.csv("./data/FixedIncomePortfolio2.csv")

# convert all NA to NULL
data[is.na(data)] <- "NULL"

# convert all 999999999's
data[data==-999999999] <- "NULL"
data[data==999999999] <- "NULL"

# change column names
colnames(data)[colnames(data)=="LegalEntityIDCounterparty"] <- "CounterpartyID"
colnames(data)[colnames(data)=="FixingDays"] <- "FixingPeriod"
colnames(data)[colnames(data)=="MarketObjectCodeRateReset"] <- "MarketObjectCodeOfRateReset"

# remove all T00's
idx_t00 <- grep("Date", colnames(data))
for (i in 1:length(idx_t00)) {
  data[,idx_t00[i]] <- gsub('T00','',data[,idx_t00[i]])
}

# transform certain cycles...
idx_cycle <- grep("Cycle", colnames(data))
for (i in 1:length(idx_cycle)) {
  data[,idx_cycle[i]] <- gsub("1M-","P1ML1",data[,idx_cycle[i]], fixed=TRUE)
  data[,idx_cycle[i]] <- gsub("1Y-","P1YL1",data[,idx_cycle[i]], fixed=TRUE)
  data[,idx_cycle[i]] <- gsub("1Y+","P1YL0",data[,idx_cycle[i]], fixed=TRUE)
  data[,idx_cycle[i]] <- gsub("2Y+","P2YL0",data[,idx_cycle[i]], fixed=TRUE)
  data[,idx_cycle[i]] <- gsub("6M+","P6ML0",data[,idx_cycle[i]], fixed=TRUE)
}

# convert DayCountConventions
idx_daycount <- grep("DayCountConvention", colnames(data))
data[,idx_daycount] <- gsub("30E/360", "30E360", data[,idx_daycount], fixed=TRUE)
data[,idx_daycount] <- gsub("A/AISDA", "AA", data[,idx_daycount], fixed=TRUE)

# convert calendar entries
idx_calendar <- grep("Calendar", colnames(data))
data[,idx_calendar] <- gsub("NOCALENDAR", "NC", data[,idx_calendar], fixed=TRUE)
data[,idx_calendar] <- gsub("WEEKDAY", "MF", data[,idx_calendar], fixed=TRUE)

# convert certain variables
idx_cycleir <- grep("CyclePointOfInterestPayment", colnames(data))
data[,idx_cycleir] <- gsub("EndOf", "E", data[,idx_cycleir], fixed=TRUE)
idx_cyreset <- grep("CyclePointOfRateReset", colnames(data))
data[,idx_cyreset] <- gsub("BeginningOf", "B", data[,idx_cyreset], fixed=TRUE)
idx_fix <- grep("FixingPeriod", colnames(data))
data[,idx_fix] <- gsub("0D", "P0D", data[,idx_fix], fixed=TRUE)

# add a column for redemption payments of LAMs


write.csv(data, "./data/FixedIncomePortfolio2.csv")


