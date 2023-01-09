# test script
library(ggpubr); library(car); library(xtable)



# Read Data ---------------------------------------------------------------

setwd("GitHub/-02441-case-project/Case1/scripts")

data <- read.table("SPR.txt",header=TRUE)



# Data Exploration --------------------------------------------------------

pairs(data)


# Question One ------------------------------------------------------------

model <- lm(Response ~ EnzymeConc + CaStock,data)

summary(model)

Anova(model)

# base on this model, it does not seem that the hardness of the water has a signifiant effect on the response variable

shapiro.test(data$Response[data$CaStock == "Ca+"])
shapiro.test(data$Response[data$CaStock == "Ca0"])

t.test(data$Response[data$CaStock == "Ca+"],data$Response[data$CaStock == "Ca0"])



# Question Two ------------------------------------------------------------


