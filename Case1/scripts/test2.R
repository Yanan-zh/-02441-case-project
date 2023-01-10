# test script
library(tidyverse); library(ggpubr); library(car)

# Data loading
data <- read.csv("Case1/scripts/SPR.txt", sep="")
data$Enzyme = as.factor(data$Enzyme)
data$DetStock = as.factor(data$DetStock)
data$CaStock = as.factor(data$CaStock)
data$EnzymeConc = as.factor(data$EnzymeConc)
data$sqrtResponse = sqrt(data$Response)
str(data)


# Visualisation
normal_plot <- function(col){
  par(mfrow=c(1,3))
  x <- c(unlist(data[col]))
  hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Histogram of", col))
  x_range <- seq(min(x), max(x), length.out = 100)
  lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
  plot(ecdf(x), main = paste0("ecdf(", col, ")"))
  lines(x_range, pnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
  abline(v=hist1$breaks)
  qqnorm(x); qqline(x)
}
data$Response2 = sqrt(data$Response)
normal_plot("Response2")
shapiro.test(data$Response2)

# library(MESS)
# qqwrap <- function(x, y, ...){
#   stdy <- (y-mean(y))/sd(y)
#   qqnorm(stdy, main="", ...)
#   qqline(stdy)}
# wallyplot(data$Response2, FUN=qqwrap, ylim=c(-3,3))

# Histogram of Response for dif. CaStock
par(mfrow=c(1,2))
x <- data$Response[data$CaStock == "Ca+"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Ca+"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
x <- data$Response[data$CaStock == "Ca0"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Ca0"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")

# Histogram of Response for dif. DetStock
par(mfrow=c(1,2))
x <- data$Response[data$DetStock == "Det+"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Det+"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
x <- data$Response[data$DetStock == "Det0"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Det0"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
shapiro.test(data$Response[data$DetStock == "Det+"])

# Models: Question 1
lm1 <- lm(Response ~ DetStock*CaStock, data)
summary(lm1)
Anova(lm1)
# 50.6% explained variation
# p-values:
# (Intercept)               < 2e-16
# DetStockDet0              = 4.35e-15
# CaStockCa0                = 0.692
# DetStockDet0:CaStockCa0   = 0.733
par(mfrow=c(2,2))
plot(lm1)

# Looks nice.
drop1(lm1, test = "F")
# Dropping the interaction
lm2 <- lm(Response ~ DetStock + CaStock, data)
summary(lm2)
# 50.6% explained variation
# p-values
# (Intercept)               < 2e-16
# DetStockDet0              < 2e-16
# CaStockCa0                = 0.826
plot(lm2)
# Highly similar to before
drop1(lm2, test = "F")
# Dropping CaStock (not significant)
lm3 <- lm(Response ~ DetStock, data)
summary(lm3)
# 50.6% explained variation
# p-values
# (Intercept)               < 2e-16
# DetStockDet0              < 2e-16
plot(lm3)
# ANOVA
Anova(lm3)

lm3_sqrt <- lm(sqrtResponse ~ DetStock, data)
summary(lm4)
# 56.7% explained variation
# p-values
# (Intercept)               < 2e-16
# DetStockDet0              < 2e-16
plot(lm4)
# ANOVA
Anova(lm3)

# Highly significant.
par(mfrow=c(1,1))
data %>%
  ggplot(mapping = aes(x = DetStock,
                       y = Response,
                       fill = DetStock)) +
  geom_boxplot() +
  theme_minimal()
# The presence of a detergent enhances the catalytic activity
data %>%
  ggplot(mapping = aes(x = CaStock,
                       y = Response,
                       fill = CaStock)) +
  geom_boxplot() +
  theme_minimal()

# Enzyme type
data %>%
  filter(CaStock == "Ca0") %>% 
  ggplot(mapping = aes(x = Enzyme,
                       y = Response,
                       fill = Enzyme)) +
  geom_boxplot() +
  theme_minimal() +
  geom_pwc()



library(MESS)
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
wallyplot(lm3$residuals, FUN=qqwrap, ylim=c(-3,3))

lm4 <- lm(sqrtResponse ~ DetStock, data)
summary(lm4)
plot(lm4)
wallyplot(lm4$residuals, FUN=qqwrap, ylim=c(-3,3))


lm_enzyme <- lm(Response ~ EnzymeConc, data)
summary(lm_enzyme)
plot(lm_enzyme)
data$factEnzymeConc = as.factor(data$EnzymeConc)
lm_enzyme2 <- lm(Response ~ factEnzymeConc, data)
summary(lm_enzyme2)
plot(lm_enzyme2)
data %>%
  ggplot(mapping = aes(x = as.factor(EnzymeConc),
                       y = Response,
                       fill = as.factor(EnzymeConc))) +
  geom_boxplot() +
  stat_compare_means() +
  geom_pwc()
kruskal.test(Response ~ EnzymeConc, data)
4*(4-1)/2
0.05/6


# Check for interactions
?interaction.plot
interaction.plot(data$EnzymeConc, data$DetStock, data$Response, col = c(1:2))
interaction.plot(data$EnzymeConc, data$CaStock, data$Response, col = c(3:4))
interaction.plot(data$CaStock, data$DetStock, data$Response, col = c(1:2))
interaction.plot(data$EnzymeConc, data$Enzyme, data$Response, col = c(5:9))
# There seems to be some interaction between Enzyme and EnzymeConc
# Let us investigate
lm_EE1 <- lm(Response ~ Enzyme*EnzymeConc, data)
summary(lm_EE1)
Anova(lm_EE1)
drop1(lm_EE1, test = "F")
lm_EE2 <- lm(Response ~ Enzyme + EnzymeConc, data)
summary(lm_EE2)
drop1(lm_EE2, test = "F")
# Nothing more to drop
# Test the same thing but with sqrt(Response)
lm_EE3 <- lm(sqrtResponse ~ Enzyme*EnzymeConc, data)
summary(lm_EE3)
# No interactions of significance (enzyme D has an interaction with EnzymeConc. tho)

# Creating a final model
lm_final1 <- lm(Response ~ Enzyme*EnzymeConc*DetStock, data)
summary(lm_final1)
# 96.5% explained variation
plot(lm_final1)

lm_final2 <- lm(sqrtResponse ~ Enzyme*EnzymeConc*DetStock, data)
summary(lm_final2)
# 96.8% explained variation
plot(lm_final2)
drop1(lm_final2, test = "F")

lm_final3 <- lm(sqrtResponse ~ Enzyme + EnzymeConc + DetStock +
                  Enzyme*EnzymeConc + Enzyme*DetStock +
                  EnzymeConc*DetStock,
                data)
summary(lm_final3)
# 96.6% explained variation
plot(lm_final3)
drop1(lm_final3, test = "F")
# Least significant = EnzymeConc:DetStock (pval = 0.392808)

lm_final4 <- lm(sqrtResponse ~ Enzyme + EnzymeConc + DetStock +
                  Enzyme*EnzymeConc + Enzyme*DetStock,
                data)
summary(lm_final4)
# 96.5% explained variation
plot(lm_final4)
drop1(lm_final4, test = "F")
Anova(lm_final4)

# Outliers: Observations no. 147, 158, and 160
# Remove outliers and check model again (+ check normality)
data = data[-147,]; data = data[-157,]; data = data[-158,]
lm_final5 <- lm(sqrtResponse ~ Enzyme + EnzymeConc + DetStock +
                  Enzyme*EnzymeConc + Enzyme*DetStock,
                data)
summary(lm_final5)
plot(lm_final5)


hist(data$sqrtResponse)
normal_plot("sqrtResponse")
str(data)
shapiro.test(data$sqrtResponse)
data = data[-147,]
data[147,]
par(mfrow=c(2,2))
