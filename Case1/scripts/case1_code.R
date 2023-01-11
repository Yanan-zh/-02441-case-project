# test script
library(tidyverse); library(ggpubr); library(car); library(xtable)

# Data loading
data <- read.csv("Case1/SPR.txt", sep="")
data$Enzyme = as.factor(data$Enzyme)
data$DetStock = as.factor(data$DetStock)
data$CaStock = as.factor(data$CaStock)
data$sqrtResponse = sqrt(data$Response)
str(data)


################################################################################
### Data description & visualization
################################################################################

str(data)
summary(data)
print(xtable(summary(data), type="html", file="xt.html", include.rownames=FALSE))

ggplot(data = data,mapping =aes(Enzyme, Response, fill=as.factor( EnzymeConc)))+
  geom_boxplot()+ guides(fill=guide_legend(title="EnzymeConc"))+ theme_classic()+ stat


ggplot(data = data,mapping= aes(DetStock,Response,fill=DetStock))+
  geom_boxplot()+ theme_classic()+
  stat_compare_means()


ggplot(data = data,mapping =aes(CaStock ,Response,fill=CaStock))+
  geom_boxplot()+theme_classic()+ stat_compare_means()

data %>% 
  ggplot(mapping = aes(x=Enzyme,
                       y=Response,
                       fill=as.factor(EnzymeConc))) +
  geom_boxplot() +
  theme_classic() +
  stat_compare_means(label.y = c(1588+20,1000.5+20,1133.7+20,742.1+20,1162.7+20)) +
  xlab("Enzyme type") +
  labs(fill = "Enzyme concentration") +
  ylim(0,1610)
ggsave("Case1/figures/boxplot_enz_conc_kruskal.png", dpi = 300, width = 3200, height = 1200, units = "px")

################################################################################
### Model-building
################################################################################

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
# lamada = 4545455, near 0.5 so squre root

data$new_response <- sqrt(data$Response)


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
lm2 = lm(new_response ~ new_EnzymeConc*Enzyme*DetStock*CaStock+Cycle, data)
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


shapiro.test(lm4.5$residuals)

bc = boxCox(lm4.5, lambda = seq(0,2,0.05))
lambda= bc$x[ which.max(bc$y)]

par(mfrow=c(2,2))
plot(lm4.5)



################################################################################
### Model visualization
################################################################################

# Plotting function
sub_plot <- function(){
  par(mfrow=c(2,3))
  for (enzyme in sort(unique(data$Enzyme))){
    data_sub <- data %>% 
      filter(Enzyme == enzyme)
    
    new.data <- data.frame("Enzyme" = rep(c(enzyme), each = 1000),
                           "DetStock" = rep(c("Det+", "Det0"), each = 500),
                           "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                           "new_EnzymeConc" = seq(min(data_sub$new_EnzymeConc), max(data_sub$new_EnzymeConc), length = 100))
    
    pred.int <- predict(lm4.5, new.data, interval = "prediction")
    
    plot(new_response ~ new_EnzymeConc, data_sub, col = as.numeric(data_sub$DetStock),
         pch = 19, main = paste("Enzyme", enzyme), ylim = c(-2,42),
         xlab = "Enzyme concentration", ylab = "Response")
    legend("topleft", legend = c("Detergent present", "Detergent absent"),
           pch = 19, col = 1:length(unique(data_sub$DetStock)))
    matlines(new.data$new_EnzymeConc[new.data$DetStock=="Det+"], pred.int[new.data$DetStock=="Det+",],
             lty = c(1,3,3), col = 1, lw = 2)
    matlines(new.data$new_EnzymeConc[new.data$DetStock=="Det0"], pred.int[new.data$DetStock=="Det0",],
             lty = c(1,33), col = 2, lw = 2)
  }
  par(mfrow=c(1,1))
}

sub_backtransform <- function(){
  par(mfrow=c(2,3))
  for (enzyme in sort(unique(data$Enzyme))){
    data_sub <- data %>% 
      filter(Enzyme == enzyme)
    
    new.data <- data.frame("Enzyme" = rep(c(enzyme), each = 1000),
                           "DetStock" = rep(c("Det+", "Det0"), each = 500),
                           "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                           "new_EnzymeConc" = seq(min(data_sub$new_EnzymeConc), max(data_sub$new_EnzymeConc), length = 500))
    new.data$EnzymeConc = new.data$new_EnzymeConc * new.data$new_EnzymeConc
    pred.int <- predict(lm4.5, new.data, interval = "prediction")
    pred.int <- pred.int^2
    # sort.fit <- pred.int[,1] %>% sort()
    
    plot(Response ~ EnzymeConc, data_sub, col = as.numeric(data_sub$DetStock),
         pch = 19, main = paste("Enzyme", enzyme), ylim = c(0,1600),
         xlab = "Enzyme concentration", ylab = "Response")
    legend("topleft", legend = c("Detergent present", "Detergent absent"),
           pch = 19, col = 1:length(unique(data_sub$DetStock)))
    matlines(new.data$EnzymeConc[new.data$DetStock=="Det+"], pred.int[new.data$DetStock=="Det+",],
             lty = c(1,3,3), col = 1, lw = 2)
    matlines(new.data$EnzymeConc[new.data$DetStock=="Det0"], pred.int[new.data$DetStock=="Det0",],
             lty = c(1,3,3), col = 2, lw = 2)
  }
}

# Plotting
# sub_plot()
png(file = "Case1/figures/model_visualization.png", width = 2000, height = 1600, pointsize = 40)
sub_backtransform()
dev.off()



################################################################################
### Systematic errors: The small model
################################################################################


data <- read.csv("Case1/SPR.txt", sep="")
data = data [ data$EnzymeConc== 0,]

data = data[-c(40),]
data$RunDate = as.factor (data$RunDate)


lm5 = lm(Response~RunDate*Cycle*DetStock,data)

bc2 = boxCox(lm5, lambda = seq(0,0.5,0.05))
lambda = bc2$x[ which.max(bc2$y) ]
data$Response1 = (data$Response ^ lambda - 1) / lambda



lm5.1 <- lm(data$Response1~RunDate*Cycle*DetStock,data)
step(lm5.1)

lm5.2 = lm(formula = data$Response1 ~ RunDate + Cycle + DetStock + Cycle:DetStock, 
           data = data)

drop1(lm5.2,test="F")
lm5.3<- update(lm5.2, . ~ . -Cycle:DetStock)
drop1(lm5.3,test="F")

lm5.4<- update(lm5.3, . ~ . -Cycle)
drop1(lm5.4,test="F")


png("error.png", units = "in", width = 10, height = 7, res = 300)


par(mfrow=c(2,2))
plot(lm5.4, col=as.numeric(data$RunDate)+7, pch=19)
dev.off()


summary(lm5.4)
shapiro.test(lm5.4$residuals)