# test script
library(ggpubr); library(car); library(xtable)



# Read Data ---------------------------------------------------------------

setwd("GitHub/-02441-case-project/Case1/scripts")

data <- read.table("SPR.txt",header=TRUE)
data$EnzymeConc <- as.factor(data$EnzymeConc)
plot <- ggplot(data, aes(x=Response)) +   geom_histogram(aes(y = ..density..),color="black",fill="lightblue")+ geom_density()  + theme_bw() 


plot2<- ggpairs(data, aes(colour = Enzyme))
# Data Transformation -----------------------------------------------------

modelInitial <- lm(Response ~ EnzymeConc, data)

bcInitial<-boxCox(modelInitial, lambda =seq(0, 1, by = 0.05))

bcInitial$x[which.max(bcInitial$y) ]

modelSqrt <- lm(sqrt(Response) ~ EnzymeConc, data)

bcSqrt<-boxCox(modelSqrt, lambda =seq(0, 1, by = 0.05))


responseBc <- (data$Response^(0.3232323) -1)/0.3232323

modelBc <- lm(responseBc ~ EnzymeConc, data)


bcBc <-boxCox(modelSqrt, lambda =seq(0, 1, by = 0.05))

# the conclution is that we cant reach normality, and we dont gain anything by being closer to normality

# Data Exploration --------------------------------------------------------

pairs(data)


# Question One ------------------------------------------------------------

model <- lm(Response ~ EnzymeConc + CaStock,data)

summary(model)

Anova(model)

# base on this model, it does not seem that the hardness of the water has a signifiant effect on the response variable

shapiro.test(data$Response[data$CaStock == "Ca+"])
shapiro.test(data$Response[data$CaStock == "Ca0"])

# we can see that within the two groups 
wilcox.test(data$Response[data$CaStock == "Ca+"],data$Response[data$CaStock == "Ca0"])

# for funnzies checking the detergent

wilcox.test(data$Response[data$DetStock == "Det+"],data$Response[data$DetStock == "Det0"])

# looking at the enzymes

kruskal.test(Response ~ Enzyme, data) 

kruskal.test(Response ~ EnzymeConc, data) 

# systematic error model ------------------------------------------------------------

data$Cycle <- as.factor(data$Cycle)
model <- lm(sqrt(data$Response) ~ data$RunDate)

dataSub <- data[data$EnzymeConc==7.5 & data$DetStock=="Det+" & data$CaStock =="Ca+",]
mean(dataSub$Response)
model <-lm(sqrt(dataSub$Response)~dataSub$RunDate)
drop1(model,test="F")
model<- update(model, . ~ . - dataSub$Cycle31)
drop1(model,test="F")
