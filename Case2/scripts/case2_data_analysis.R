################################################################################
################################################################################
### Data description and analysis
################################################################################
################################################################################

# Loading libraries
library(tidyverse); library(car); library(ggpubr); library(ggrepel);
library(rlang); library(data.table); library(RColorBrewer)



################################################################################
### Data loading and wrangling
################################################################################

data <- read.csv("Case2/data/merged_data.csv")
data$ID <- as.factor(data$ID)
data$dir <- as.factor(data$dir)
data$cond <- as.factor(data$cond)
data$fog <- as.factor(data$fog)
data$rain <- as.factor(data$rain)
data$tempDiff <- 21 - data$temp
data$weekday <-weekdays(as.Date(data$date)); data$weekday = as.factor(data$weekday)
data$weekend <- as.factor((data$weekday == "Saturday" | data$weekday == "Sunday"))
data$consumption <- data$consumption +0.00001
data$date = as.IDate(data$date)

# Read the meta df
meta <- fread("Case2/data/metadata.csv") %>% 
  select(-Anvendelse) %>% 
  mutate(ID = as.factor(ID))

# Merge dataframes and handle NAs
data <- left_join(data, meta, by = "ID") %>% 
  mutate(building_type = replace_na(building_type, "Unknown building type"))
data$building_type = as.factor(data$building_type)



################################################################################
### Plotting functions
################################################################################

# Scatter plot function (by indexes)
# Takes as idx input either an index, a range of indexes ( c(30,35) ) or the ID
scat_plot <- function(idx = "1",
                      x_ = "tempDiff",
                      y_ = "consumption",
                      col_ = "weekend",
                      data_=data,
                      label_= NULL,
                      shape_ = NULL){
  
  
  # Define data
  buildings <- sort(unique(as.character(data_$ID)))
  
  
  # Handle ranges of indexes or multiple IDs as input
  if (length(idx) > 1){
    if (idx[1] > 100){
      sub_data <- data_ %>% 
        filter(ID %in% idx)
    } else {
      n1 <- idx[1]; n2 <- idx[2];
      sub_data <- data_ %>% 
        filter(ID %in% buildings[idx[1]:idx[2]])
    }
  } else {
    # Handle a single index as input
    if (idx < 100){
      sub_data <- data_ %>% 
        filter(ID %in% buildings[idx])
    } else {
      # Handle an ID as input
      sub_data <- data_ %>% 
        filter(ID == idx)
    }
  }
  
  
  # Plot
  actual_plot_function(sub_data_=sub_data,
                       x__ = x_,
                       y__ = y_,
                       col__ = col_,
                       label__ = label_,
                       shape__ = shape_)
}

# The function used to create the actual plot
actual_plot_function <- function(sub_data_=data,
                                 x__ = "tempDiff",
                                 y__ = "consumption",
                                 col__ = NULL,
                                 label__ = "ID",
                                 shape__ = NULL){
  if (col__ == "date"){
    sub_data_ <- sub_data_ %>% 
      mutate(date = as.POSIXct(date))
  }
  
  # Plot
  p <- ggplot(sub_data_,
              mapping = aes_string(x = {{x__}},
                                   y = {{y__}},
                                   col = {{col__}},
                                   label = {{label__}},
                                   shape = {{shape__}})) +
    geom_point() +
    theme_classic() +
    guides(shape = guide_legend(order = 2),
           label = guide_legend(order = 3))
  
  if (!(is.null(label__))){
    p <- p + geom_text_repel()
  }
  
  if(col__ == "date"){
    p <- p +
      scale_color_continuous("Date",
                             type = "viridis",
                             breaks = c(as.POSIXct("2018-09-01 CEST"),
                                        as.POSIXct("2018-10-01 CEST"),
                                        as.POSIXct("2018-11-01 CEST"),
                                        as.POSIXct("2018-12-01 CEST"),
                                        as.POSIXct("2018-12-28 CEST")),
                             labels = c("1/09",
                                        "1/10",
                                        "1/11",
                                        "1/12",
                                        "28/12"))
  }
  
  if(x__ == "tempDiff"){
    p <- p + labs(x = "Temperature difference")
  }
  if(y__ == "consumption"){
    p <- p + labs(y = "Consumption")
  }
  
  p
}



################################################################################
### Descriptive plots
################################################################################

# Create single plots for all IDs
for (id in ids){
  scat_plot(id, col = "weekend", shape_ = "building_type")
  ggsave(paste0("Case2/figures/explorative_plots/", id, "_scatter_weekend.png"),
         dpi = 300, width = 2000, height = 1600, units = "px")
}



# Plot of all data points
scat_plot(c(1,83), col_ = "ID") +
  theme(legend.position = "none") +
  ggtitle("Consumption versus temperature difference for different buildings")
ggsave("Case2/figures/scat_all.png",
       dpi = 300, width = 2000, height = 1200, units = "px")



# Candidates for weekend correlation:
# 6842421, after school care <- good
# 69688095, day care <- good
# 6790785, day care <- semi
# 69001263, day care <- semi

p1 <- scat_plot(c(6842421,
                  69688095
), col = "weekend", shape_ = "building_type") + 
  scale_colour_discrete("Weekend") +
  scale_shape("Building type") +
  ggtitle("A")



# Candidates for no weekend correlation:
# 4887707, long-term housing
# 4962433, fitness gym
# 5037175, long-term housing

p2 <- scat_plot(c(4887707
), col = "weekend", shape_ = "building_type") + 
  scale_colour_discrete("Weekend") +
  scale_shape("Building type") +
  ggtitle("B")

p3 <- scat_plot(c(4962433
), col = "weekend", shape_ = "building_type") + 
  scale_colour_discrete("Weekend") +
  scale_shape("Building type") +
  ggtitle("C")



# Candidates for three slopes
# 65118755, apartment <- unknown trend
# 65118764, day care private <- unknown trend
p4 <- scat_plot(65118755, col_ = "date", shape_ = "building_type") +
  scale_shape("Building type") +
  ggtitle("D") +
  guides(shape = guide_legend(order = -1))



# Candidates for off-season trend:
# 6392172, day care <- dates: 10-25, 10-29, 11-15, 11-22, 11-27
# 65118848, youth centre <- indoor tennis gym, dates = early dates

tennis <- data
tennis$building_type[tennis$ID == 65118848] = "Tennis court"
p5 <- scat_plot(c(65118848),
                data_ = tennis,
                col_ = "date", shape_ = "building_type") +
  scale_shape("Building type") +
  ggtitle("E")



# Candidates for no correlation between consumption and temperature difference:
# 69999051, after school care
p6 <- scat_plot(69999051, col_ = "date", shape_ = "building_type") +
  scale_shape("Building type") +
  ggtitle("F")



# Plot of data trends
ggarrange(p1,p2,p3,p4,p5,p6)
ggsave(paste0("Case2/figures/scatter_weekend_6plts.png"),
       dpi = 300, width = 4000, height = 2400, units = "px")



################################################################################
### Model building
################################################################################

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

final_model <- update(final_model,. ~ . +rain)
drop1(final_model, test = "F", k = log(nrow(data)))

final_model <- update(final_model,. ~ . -rain)



################################################################################
### Model analysis
################################################################################

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

coef$n <- n


coef$upper <- coef$Slope  +  qt(0.975,df=coef$n-83)*coef$sd.error
coef$lower <- coef$Slope  -  qt(0.975,df=coef$n-83)*coef$sd.error


# visualization -----------------------------------------------------------




# sort by slope

top10 <- ggplot(coef[0:10,], aes(x = reorder(ID, -Slope), y=Slope))+ 
  geom_bar(position=position_dodge(), stat="identity", col = "black", fill = "#F8766D") +
  geom_errorbar(aes(ymin=lower, ymax=upper)) + xlab("Building IDs") + ylab("Slope") +
  theme_classic(base_size = 13)



ggsave("Case2/figures/top10_bar.pdf", top10)