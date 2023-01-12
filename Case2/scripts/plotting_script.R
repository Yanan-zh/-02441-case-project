source("Case2/scripts/plot_functions.R")
library(tidyverse); library(car); library(ggpubr); library(ggrepel)

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
data$weekday <-weekdays(as.Date(data$date))
data$weekend <- as.factor((data$weekday == "Saturday" | data$weekday == "Sunday"))
data$consumption <- data$consumption +0.00001

summary(data)
# outlier removal
data <- data[-c(3357,3282),]

################################################################################
### Plot tests
################################################################################
scatter_plot <- function(n1, n2=NULL, data){
  buildings <- sort(unique(data$ID))
  if ()
  sub_data <- data %>% 
    filter(ID %in% buildings[1:2])
}

n2 <- NULL

sub_data %>% 
  ggplot(mapping = aes(x = tempDiff,
                       y = consumption,
                       col = ID,
                       label = date)) +
  geom_point() +
  theme_classic() +
  geom_text_repel()


# Outliers (ID, date)
# 4962433,  2018-12-09
# 5140250,  2018-12-12