library("ggplot2")
library("tidyverse")

# make sure to have final_model from analysis

summary(final_model)

Anova(final_model)


glm(final_model)


# conf int ----------------------------------------------------------------

#frist 83

A_1 <- matrix(0,83,83)

#83
#for tempDiff

A_2 <- matrix(1,83,1)

#84
#for weekendTrue, windspeed

A_3 <- matrix(0,83,2)

#86

#for direction

A_4 <- matrix(0,83,15)

# 101
# for humiditiy

A_5 <- matrix(0,83,1)

# 102

# for interaction id:tempDIFF

A_6_top <-matrix(0,1,82)

A_6_bot <-matrix(diag(82),82,82)

A_6 <- rbind(A_6_top,A_6_bot)

# 184

# for ID:weekend interaction -> should be zeros

A_7 <-matrix(0,83,82)

# 266

# for non id interactions weekend:windspeed, tempDiff:windspeed -> mean(wind_spd)

A_8 <- matrix(0,83,1)

A_9 <- matrix(mean(data$wind_spd),83,1)

A_10 <- matrix(0,83,1)

# 269 

# for dir:wind 

A_11 <- matrix(0,83,15)

# 284
#for windspd:hum

A_12 <- matrix(0,83,1)

#285

# design matrix

model_sum <- summary(final_model,correlation = TRUE)

A <- cbind(A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8,A_9,A_10,A_11,A_12)

est <- A%*%model_sum$coefficients[,1]

var_est <-A%*%model_sum$cov.unscaled%*% t(A)*model_sum$sigma^2

coef <-data.frame(ID=levels(data$ID), Slope = est, sd.error=sqrt(diag(var_est)))
  
n <- c()

for(id in coef$ID){
  print(nrow(data[data$ID==id,]))
  n <-append(n,nrow(data[data$ID==id,]))
}
  
coef$n <- 


coef$upper <- coef$Slope  +  qt(0.975,df=9785-285)*coef$sd.error
coef$lower <- coef$Slope  -  qt(0.975,df=9785-285)*coef$sd.error


# visualization -----------------------------------------------------------


# sort by slope

top10 <- ggplot(coef[0:10,], aes(x = reorder(ID, -Slope), y=Slope))+ 
  geom_bar(position=position_dodge(), stat="identity", col = "black", fill = "#F8766D") +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + xlab("Building IDs") + ylab("Slope") +
  theme_classic(base_size = 13)




ggsave("Case2/figures/top10_bar.pdf", top10)



# yanan's---------------------------
# load metadata
metadata <- read.csv("Case2/data/metadata.csv")[-92,]
metadata$ID = as.character(metadata$ID)

#sort
coef_sort <- coef[order(-coef$Slope),]

# merge
top_building = left_join(coef_sort,metadata,by = 'ID')


# visialization
png("slope.png", units = "in", width = 10, height = 7, res = 300)
ggplot(top_building[0:10,], aes(x = reorder(ID, -Slope), y=Slope, fill = building_type))+ 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + xlab("Building IDs") + ylab("Slope") +
  theme_classic(base_size = 13)+
  theme(legend.position = "bottom")

dev.off()

# visialization
png("slopeAll.png", units = "in", width = 10, height = 7, res = 300)
ggplot(top_building, aes(x = reorder(ID, -Slope), y=Slope, fill = "#F8766D"))+ 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + xlab("Building IDs") + ylab("Slope") +
  theme_classic(base_size = 13)+
  theme(legend.position = "none",axis.text.x=element_blank(),axis.ticks.x=element_blank())

dev.off()




