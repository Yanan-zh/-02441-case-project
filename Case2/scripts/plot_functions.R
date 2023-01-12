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
### Functions
################################################################################

# Scatter plot function
scatter_plot <- function(x_ = "tempDiff",
                         y_ = "consumption",
                         col_ = "ID",
                         data_=data,
                         ran = c(1,83),
                         label_="nope"){
  
  # Define data
  buildings <- sort(unique(data_$ID))
  
  
  n1 <- ran[1]; n2 <- ran[2];
  sub_data <- data_ %>% 
    filter(ID %in% buildings[n1:n2])
  
  # # Handle special case
  if (label_ == "nope"){
    p <- ggplot(sub_data,
                mapping = aes_string(x = x_,
                                     y = y_,
                                     col = col_))
  } else {
    p <- ggplot(sub_data,
                mapping = aes_string(x = x_,
                                     y = y_,
                                     col = col_,
                                     label = label_))
  }
  
  p <- p + geom_point() +
    theme_classic()
  
  if (!(label_ == "nope")){
    p <- p + geom_text_repel()
  }
  p
}