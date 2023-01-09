# test script
library(tidyverse); library(ggpubr); library(car)

# Data loading
data <- read.csv("Case1/SPR.txt", sep="")
data$Enzyme = as.factor(data$Enzyme)
data$DetStock = as.factor(data$DetStock)
data$CaStock = as.factor(data$CaStock)
data$sqrtResponse = sqrt(data$Response)
str(data)

# Achieving near-normality
data = data[-147,] # remove outlier
normal_plot("EnzymeConc")
par(mfrow=c(1,1))
boxCox(lm(Response ~ EnzymeConc, data), lambda = seq(0,0.5,0.05))
# lambda = 0.32ish -> sqrt-transformation

lm2 <- lm(Response ~ EnzymeConc*Enzyme*DetStock*CaStock*RunDate*Cycle, data)
summary(lm2)
boxCox(lm2, lambda=seq(-0.5,1.5,0.05))
# lambda = 0.5 -> sqrt-transformation

lm3 <- lm(sqrtResponse ~ EnzymeConc*Enzyme*DetStock*CaStock*RunDate*Cycle, data)
boxCox(lm3, lambda=seq(0.5,1.5,0.05))
# lambda = 1.0 -> the transformation worked. We can assume normality.

# Trying to model EnzymeConc in order to determine the correct way to transform
# it
lm4 <- lm(EnzymeConc ~ Response, data)
boxCox(lm4, lambda=seq(-0.5,0.5,0.05))
par(mfrow=c(2,2))
plot(lm4)

# Model selection (backwards)
# Reduce lm3
step(lm3)
lm_5 <- lm(formula = sqrtResponse ~ EnzymeConc + Enzyme + DetStock + 
                 CaStock + EnzymeConc:Enzyme + Enzyme:DetStock + EnzymeConc:CaStock, 
               data = data)
summary(lm_5)
drop1(lm_5)

lm_6 <- update(lm_5, ~ . -EnzymeConc:CaStock)
summary(lm_6)
drop1(lm_6, test = "F")

lm_7 <- update(lm_6, ~ . -CaStock)
summary(lm_7)
drop1(lm_7, test = "F")

par(mfrow = c(2,2))
plot(lm_7)
