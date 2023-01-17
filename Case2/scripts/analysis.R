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

for(i in unique(data$ID)){
  mean <- mean(data[data$ID ==i,]$consumption)
  
  data[data$ID ==i,]$consumption <- data[data$ID ==i,]$consumption/mean
}
  

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

data <- data[-c(3357),]
rownames(data) <- NULL  

# remake the model

model_wInter <- lm(consumption~ ID+ tempDiff +ID:tempDiff,data)

par(mfrow=c(2,2))
plot(model_wInter, col = data$weekend)


# model with weekend ------------------------------------------------------

# so we did this entire model, and according to AIC we keep being told to keep parameters, 
# going through it again with BIC, as it is more conservative, and we have an abundance of variables.

par(mfrow=c(1,2))
plot(data$consumption~data$tempDiff, col = data$weekend)
plot(data$consumption~data$weekend)

# we are not able to see any larger difference in means, but we do a quick test anyways

wilcox.test(data[data$weekend==TRUE,]$consumption,data[data$weekend==FALSE,]$consumption)

# is does not look it, but it is significant, so we add it

model_wWeekend <- update(model_wInter,. ~ . *weekend)


# now we check to see if we should remove anything

drop1(model_wWeekend, test = "F", k = log(nrow(data)))

#so according to the BIC we remove ID:tempDiff:weekend

model_wWeekend <- update(model_wWeekend,. ~ . -ID:tempDiff:weekend)

drop1(model_wWeekend, test = "F", k = log(nrow(data)))

# no more dropping! We check Anova

Anova(model_wWeekend)

#alrighty we like the look of that!

par(mfrow=c(2,2))
plot(model_wWeekend, col = data$ID)


model_wWeekend<- lm(formula = consumption ~ ID + tempDiff + weekend + ID:tempDiff + 
     ID:weekend + tempDiff:weekend, data = data)

par(mfrow=c(2,2))
plot(model_wWeekend, col = data$ID)

# model with weather variables --------------------------------------------

# by logic- cold wind probably means we use more energy to heat buildings?

#make a better plot !!!
par(mfrow=c(1,1))
plot(data$consumption ~data$wind_spd, col = data$ID)

# it maybe looks like there are different slopes for the various buildings, we add interactions

model_wWind <-update(model_wWeekend,. ~ . *wind_spd)

drop1(model_wWind, test = "F", k = log(nrow(data)))

# makes sense, ID:weekend:wind_spd is very important

model_wWind <- update(model_wWind,. ~ . - ID:weekend:wind_spd)

drop1(model_wWind, test = "F", k = log(nrow(data)))

# remove ID:tempDiff:wind_spd

model_wWind <- update(model_wWind,. ~ . - ID:tempDiff:wind_spd)

drop1(model_wWind, test = "F", k = log(nrow(data)))

# remove ID:wind_spd

model_wWind <- update(model_wWind,. ~ . - ID:wind_spd)

drop1(model_wWind, test = "F", k = log(nrow(data)))

# remove tempDiff:weekend:wind_spd
model_wWind <- update(model_wWind,. ~ . - tempDiff:weekend:wind_spd)

drop1(model_wWind, test = "F", k = log(nrow(data)))




par(mfrow=c(2,2))
plot(model_wWind, col = data$date)


#looks like we got some interaction?

model_wWind <- update(model_wWind,. ~ . +dir + wind_spd:dir + wind_spd:dir:ID +wind_spd:dir:tempDiff)

drop1(model_wWind, test = "F", k = log(nrow(data)))

#according to this we do not want to remove anything else

par(mfrow=c(2,2))
plot(model_wWind)


#we do some analysis on the model
Anova(model_wWind)

#shows we have some multicolinearity, lets track that down 
alias(model_wWind)

# we try and remove tempDiff:wind_spd:dir
model_wWind <- update(model_wWind,. ~ . - tempDiff:wind_spd:dir)

Anova(model_wWind)

# visual analysis 

par(mfrow=c(2,2))
plot(model_wWind)



# should we add more? -----------------------------------------------------

# next thing that makes the most sense from the weather stuff is the humididity

model_wHum <- update(model_wWind,. ~ . +hum)
drop1(model_wHum, test = "F", k = log(nrow(data)))

#according to the BIC, it makes sense to add ! we check for multicplinearity

Anova(model_wHum)

# we try and add some interactions

model_wHum <- update(model_wHum,. ~ . +hum:wind_spd +hum:ID)
drop1(model_wHum, test = "F", k = log(nrow(data)))


model_wHum <- update(model_wHum,. ~ . -ID:hum)
drop1(model_wHum, test = "F", k = log(nrow(data)))


# we keep these! maybe there are some buildings which have issues with windspeed and hum in certain buildings

model_wHum <- update(model_wHum,. ~ . +hum:wind_spd:ID)
drop1(model_wHum, test = "F", k = log(nrow(data)))

# no 


model_wHum <- update(model_wHum,. ~ . -hum:wind_spd:ID)
drop1(model_wHum, test = "F", k = log(nrow(data)))


# final model -------------------------------------------------------------

final_model <- model_wHum

par(mfrow=c(2,2))
plot(final_model)

data <- data[-c(8666,8926,7811),]
rownames(data) <- NULL  
final_model <-lm(formula = consumption ~ ID + tempDiff + weekend + wind_spd + 
                   dir + hum + ID:tempDiff + ID:weekend + tempDiff:weekend + 
                   tempDiff:wind_spd + weekend:wind_spd + wind_spd:dir + wind_spd:hum, 
                 data = data)

par(mfrow=c(2,2))
plot(final_model)


data <- data[-c(3357,8665,8924),]
rownames(data) <- NULL

final_model <-lm(formula = consumption ~ ID + tempDiff + weekend + wind_spd + 
                   dir + hum + ID:tempDiff + ID:weekend + tempDiff:weekend + 
                   tempDiff:wind_spd + weekend:wind_spd + wind_spd:dir + wind_spd:hum, 
                 data = data)

par(mfrow=c(2,2))
plot(final_model)

data <- data[-c(3282,7310,7810),]
rownames(data) <- NULL

final_model <-lm(formula = consumption ~ ID + tempDiff + weekend + wind_spd + 
                   dir + hum + ID:tempDiff + ID:weekend + tempDiff:weekend + 
                   tempDiff:wind_spd + weekend:wind_spd + wind_spd:dir + wind_spd:hum, 
                 data = data)

par(mfrow=c(2,2))
plot(final_model)


drop1(final_model, test = "F", k = log(nrow(data)))


