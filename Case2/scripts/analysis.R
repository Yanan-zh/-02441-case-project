setwd("/Users/astrid_harsaae/Documents/GitHub/-02441-case-project")

library(car)

# data --------------------------------------------------------------------

data <- read.csv("Case2/data/merged_data.csv")
data$ID <- as.factor(data$ID)
data$dir <- as.factor(data$dir)
data$cond <- as.factor(data$cond)
data$fog <- as.factor(data$fog)
data$rain <- as.factor(data$rain)
data$tempDiff <- 21 - data$temp
data$weekday <-weekdays(as.Date(data$date))
data$weekend <- as.factor((data$weekday == "Saturday" | data$weekday == "Sunday"))
data$consumption <- data$consumption +0.00001

summary(data)

# fix dates ---------------------------------------------------------------

uniqueDates <- unique(data$date)

ordering <- seq(1,118,1)

# initial visualization ---------------------------------------------------

pairs(data)


# initial model -----------------------------------------------------

model_initial <- lm(consumption~ ID+ tempDiff,data)

par(mfrow=c(1,1))
plot(data$consumption~data$tempDiff, col = data$ID)

# we can see there are different slopes for the different value, so we add the interaction

# model with interaction 

model_wInter <- lm(consumption~ ID+ tempDiff +ID:tempDiff,data)

par(mfrow=c(2,2))
plot(model_wInter)

# we can see that there are some outliers, we don't like this

data <- data[-c(3357,3282),]

# remake the model

model_wInter <- lm(consumption~ ID+ tempDiff +ID:tempDiff,data)

par(mfrow=c(2,2))
plot(model_wInter, col = data$weekend)


# model with weekend ------------------------------------------------------

par(mfrow=c(1,1))
plot(data$consumption~data$tempDiff, col = data$weekend)


# model with the weekend

model_wWeekend <-lm(consumption~ weekend +ID+ tempDiff +ID:tempDiff + ID:weekend + weekend:tempDiff,data)
drop1(model_wWeekend, test = "F")

par(mfrow=c(2,2))
plot(model_wWeekend, col = data$ID)



# model with weather variables --------------------------------------------

plot(data$consumption ~data$wind_spd, col = data$ID)

model_wWind <- lm(consumption~ wind_spd +weekend +ID+ tempDiff +ID:tempDiff + ID:weekend + weekend:tempDiff +wind_spd:tempDiff+wind_spd:ID+wind_spd:weekend,data)
drop1(model_wWind, test = "F")

model_wWind <- update(model_wWind,. ~ . - wind_spd:weekend)
drop1(model_wWind, test = "F")


# add some wind direction based on some plots
par(mfrow=c(1,1))
plot(data$consumption ~data$wind_spd, col = data$dir)

model_wDir <- update(model_wWind,. ~ . +dir)
model_wDir <- update(model_wWind,. ~ . +dir:wind_spd)

par(mfrow=c(2,2))
plot(model_wDir)

model_final <- lm(consumption~ wind_spd +weekend +ID+ tempDiff +ID:tempDiff + ID:weekend + weekend:tempDiff +wind_spd:tempDiff+wind_spd:ID+dir+dir:wind_spd,data)

par(mfrow=c(2,2))
plot(model_wDir)


