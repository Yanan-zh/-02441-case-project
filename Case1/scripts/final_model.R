# test script
library(tidyverse); library(ggpubr); library(car)

# Data loading
data <- read.csv("Case1/SPR.txt", sep="")

 
data$Enzyme = as.factor(data$Enzyme)
data$DetStock = as.factor(data$DetStock)
data$CaStock = as.factor(data$CaStock)
#data$sqrtResponse = sqrt(data$Response)
str(data)

# Achieving near-normality
#data = data[-147,] # remove outlier
#normal_plot("EnzymeConc")
par(mfrow=c(1,1))

# make max model
lm1 = lm(Response ~ EnzymeConc*Enzyme*DetStock*CaStock, data)
summary(lm1)

par(mfrow=c(2,2))
plot(lm1)

shapiro.test(lm1$residuals)# residue not normally distributed
# plots also not good, need box cox


#box cox transform y and x

bc1 = boxCox(lm1, lambda = seq(0,1,0.05))
bc1$x[ which.max(bc1$y) ]
# lamada = 0.4545455, near 0.5 so squre root

data$new_response <- sqrt(data$Response)

lm1.1 = lm(new_response ~ EnzymeConc*Enzyme*DetStock*CaStock, data)

par(mfrow=c(1,1))
bc2 = boxCox(lm1.1, lambda = seq(0,2,0.05))
bc2$x[ which.max(bc2$y) ]



data$EnzymeConc[data$EnzymeConc==0.0] <- data$EnzymeConc[data$EnzymeConc==0.0]+0.000001

#data2 = data[c(4,5,6,7,8)]

# get lambda 0.59792 close to 0.5 so sqrt
boxTidwell(new_response ~ EnzymeConc, ~ Enzyme + DetStock + CaStock, data = data)

# transform x
data$new_EnzymeConc <- sqrt(data$EnzymeConc)


# after transform there is a minus value appears in new_response(-1.413877)
# but y still not normal? so assumpt its normal dist? next
shapiro.test(data$new_response)

# transform x(concentration) here? why transfer x?

plot(sqrt(data$EnzymeConc) ~ data$new_response,col="green")



# try make max model again
lm2 = lm(new_response ~ new_EnzymeConc*Enzyme*DetStock*CaStock, data)
summary(lm2)
step(lm2)
lm2.1 = lm(formula = new_response ~ new_EnzymeConc + Enzyme + DetStock + 
             CaStock + new_EnzymeConc:Enzyme + new_EnzymeConc:DetStock + 
             Enzyme:DetStock + new_EnzymeConc:CaStock + DetStock:CaStock + 
             new_EnzymeConc:DetStock:CaStock, data = data)

summary(lm2.1)
Anova(lm2.1)
drop1(lm2.1,test = 'F')

# continue drop
lm2.2 = update(lm2.1, ~. -new_EnzymeConc:DetStock:CaStock)
summary(lm2.2)
Anova(lm2.2)

drop1(lm2.2,test = 'F')

# drop again
lm2.3 = update(lm2.2, ~. -new_EnzymeConc:DetStock-DetStock:CaStock)
summary(lm2.3)
Anova(lm2.3)
drop1(lm2.3,test = 'F')

par(mfrow=c(2,2))
plot(lm2.3)

# remove outlier? 160,147
data_remove2 = data[-c(147,160),] # remove outlier

lm4 = lm(new_response ~ new_EnzymeConc*Enzyme*DetStock*CaStock, data_remove2)
summary(lm4)
step(lm4)

step(lm4,k=log(length(data_remove2)))


lm4.1 = lm(formula = new_response ~ new_EnzymeConc + Enzyme + DetStock + 
             CaStock + new_EnzymeConc:Enzyme + new_EnzymeConc:DetStock + 
             Enzyme:DetStock + new_EnzymeConc:CaStock + Enzyme:CaStock + 
             DetStock:CaStock + new_EnzymeConc:Enzyme:CaStock + new_EnzymeConc:DetStock:CaStock, 
           data = data_remove2)


drop1(lm4.1,test = 'F')

lm4.2 = update(lm4.1, ~. -new_EnzymeConc:Enzyme:CaStock-new_EnzymeConc:DetStock:CaStock)
drop1(lm4.2,test = 'F')
Anova(lm4.2)

lm4.3 = update(lm4.2, ~. -new_EnzymeConc:DetStock)
drop1(lm4.3,test = 'F')
Anova(lm4.3)

lm4.4 = update(lm4.3, ~. -DetStock:CaStock)
drop1(lm4.4,test = 'F')
Anova(lm4.4)

lm4.5 = update(lm4.4, ~. -Enzyme:CaStock)
drop1(lm4.5,test = 'F')
Anova(lm4.5)

summary(lm4.5)

shapiro.test(lm4.5$residuals)

bc = boxCox(lm4.5, lambda = seq(0,2,0.05))
lambda= bc$x[ which.max(bc$y)]

png("summery.png", units = "in", width = 10, height = 7, res = 300)


par(mfrow=c(2,2))
plot(lm4.5, col=as.numeric(data$Enzyme)+7, pch=19)
dev.off()

AIC(lm4,lm4.5)
