#####################################################
# yearFraction - Object example code
library(FEMS)

yearFraction("2020-01-01", "2020-01-31", "30E360")
yearFraction(c("2020-01-01", "2020-01-31"), "2020-01-31")
yearFraction("2020-01-01", c("2020-01-01", "2020-01-31"))
yearFraction(c("2020-01-01", "2020-01-31"), c("2020-01-31", "2020-02-28"))

# change daycount convention...
# valid ones are: "30E360", "30E360ISDA", "A360", "A365", "AA"
yearFraction("2020-01-01", "2020-01-31", "30E360")
yearFraction("2020-01-01", "2020-01-31", "30E360ISDA")
yearFraction("2020-01-01", "2020-01-31", "A360")
yearFraction("2020-01-01", "2020-01-31", "A365")
yearFraction("2020-01-01", "2020-01-31", "AA")

# one end date before start date
yearFraction(c("2020-01-01", "2020-01-31"),c("2019-01-31", "2020-02-28"))

# wrong daycount convention
# yearFraction("2020-01-01", "2020-01-31", "a") #should return error




