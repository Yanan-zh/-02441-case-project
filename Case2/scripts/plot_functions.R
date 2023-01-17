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

summary(data)
# outlier removal <- don't do that yet.
# data <- data[-c(3357,3282),]
# rownames(data) <- NULL  
# data <- data[-3438,]
# rownames(data) <- NULL 
# data <- data[-c(2535),]
# rownames(data) <- NULL  

# Read the meta df
meta <- fread("Case2/data/metadata.csv") %>% 
  select(-Anvendelse) %>% 
  mutate(ID = as.factor(ID))

# Merge dataframes and handle NAs
data <- left_join(data, meta, by = "ID") %>% 
  mutate(building_type = replace_na(building_type, "Unknown building type"))
data$building_type = as.factor(data$building_type)

################################################################################
### Functions
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