#####################################################
# DayConter - Object example code

library("devtools")
devtools::load_all()

yearFraction("2020-01-01","2020-01-31","30E/360")
yearFraction(c("2020-01-01","2020-01-31"),"2020-01-31")
yearFraction("2020-01-01",c("2020-01-01","2020-01-31"))
yearFraction(c("2020-01-01","2020-01-31"),c("2020-01-31","2020-02-28"))

# change daycount convention...
# valid ones are: "30E/360", "30E/360ISDA", "A/360", "A/365", "A/AISDA"
yearFraction("2020-01-01","2020-01-31","a")
yearFraction("2020-01-01","2020-01-31","30E/360ISDA")
yearFraction("2020-01-01","2020-01-31","A/360")
yearFraction("2020-01-01","2020-01-31","A/365")
yearFraction("2020-01-01","2020-01-31","A/AISDA")

# Test for error messages...
# one end date before start date
yearFraction(c("2020-01-01","2020-01-31"),c("2019-01-31","2020-02-28"))

# wrong daycount convention
yearFraction("2020-01-01","2020-01-31","a")




