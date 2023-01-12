setwd("/Users/astrid_harsaae/Documents/GitHub/-02441-case-project")

library(car)

# data --------------------------------------------------------------------

data <- read.csv("Case2/data/merged_data.csv")
data$date <- as.factor(data$date)
data$ID <- as.factor(data$ID)
data$dir <- as.factor(data$dir)
data$cond <- as.factor(data$cond)
data$fog <- as.factor(data$fog)
data$rain <- as.factor(data$rain)


summary(data)

# initial visualization ---------------------------------------------------

pairs(data)

# data transformation -----------------------------------------------------

# we want to transform the data such that we only have positive values
data$temp <- data$temp + 273.15

# we want non zero values in the y data

data$consumption <- data$consumption + 0.0000001

lm <- lm(consumption~ temp*ID*date*dew_pt*hum,data)

par(mfrow=c(1,1))
bc2 = boxCox(lm, lambda = seq(-10,10,0.05))
bc2$x[ which.max(bc2$y) ]


