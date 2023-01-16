library(tidyverse); library(car); library(ggpubr); library(data.table)
library(xtable)

################################################################################
### Data loading and cleansing
################################################################################

## Read meter files (b+c)
# Initialise empty data frame
columns <- c("ID", "Time", "Reading")
meter <- data.frame(matrix(nrow = 0, ncol = length(columns)))

# Read meter files into data frame
files <- list.files("Case2/data/meterdata/")
readings <- c()
for (filename in files){
  file <- fread(paste0("Case2/data/meterdata/", filename))
  readings <- c(readings, dim(file)[1])
  file <- file %>% 
    select(V1, V2, V4)
  meter <- rbind(meter,file)
}
colnames(meter) <- columns

# Reformat Readings for better numerical comparison
meter <- meter %>% 
  mutate(Reading = as.numeric(gsub(",", ".", Reading)))

# Reformat Time to YYYY-MM-DD hh:mm:ss for better numerical comparison
meter <- meter %>% 
  mutate(Time = as.POSIXct(paste0(substr(Time, 0, 13), ":", substr(Time, 15, 16)), tz = "GMT", format = "%d-%m-%Y%H:%M"))



## Exclude meters (buildings) with missing readings (d)
meter_clean <- meter
# Iterate over all meters
for (id in unique(meter$ID)){
  # Select data for the current meter
  meter_data <- meter %>% 
    filter(ID == id)
  # If missing readings, remove from meter_clean
  if (!(dim(meter_data)[1] == 121)){
    meter_clean <- meter_clean %>% 
      filter(!(ID == id))
  }
}
length(unique(meter$ID)) # from 97 original meters
length(unique(meter_clean$ID)) # to 83 meters with no missing readings



## Standardise timepoints (e, part one)
# Initialise empty data frames
columns <- c("ID", "Time", "Reading")
temp.meter <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(temp.meter) <- columns

# Define timepoints to interpolate to
timepoints <- data.frame("Time" = unique(meter_clean$Time)) %>% 
  mutate(Time = as.POSIXct(paste0(substr(Time, 0, 10), " 23:59:00"), tz = "GMT"))
timepoints <- sort(unique(timepoints[,1])) # all dates in the WG data frame
timepoints <- timepoints[1:120] # remove the last date (12-29)
# (can't be used for interpolation, as the last date in the meter data is 12-29)

# Iterate over all meters
for (id in unique(meter_clean$ID)){

  # Interpolate data
  meter_current <- meter_clean %>% 
    filter(ID == id)
  interpol.data <- approx(meter_current$Time, meter_current$Reading, xout = timepoints)
  # NAs generated 13 times (so for 13 buildings?) <-- look into this
  
  # Add data to temporary data frame
  temp.meter <- rbind(temp.meter,data.frame("ID" = id,
                                            "Time" = interpol.data$x,
                                            "Reading" = interpol.data$y))
}
# Update the data frame
meter_clean <- temp.meter



## Compute consumption for each building (e, part two)
meter_clean <- meter_clean %>% 
  mutate(date = substr(Time, 0, 10)) %>% 
  select(-Time)
dates <- unique(meter_clean$date)

# PLAN (pseudocode):
# for each id(building),
# compute the consumption per day
# (current reading - previous reading)
# if date == "2018-08-31", set consumption to 0 (or NA)
meter_clean$consumption = 0

for (id in unique(meter_clean$ID)){
  for (i in 1:length(dates)){
    if (dates[i] == "2018-08-31"){
      cons = 0
    } else {
      read_prev = meter_clean$Reading[meter_clean$date == dates[i-1]]
      read_cur = meter_clean$Reading[meter_clean$date == dates[i]]
      cons = read_cur - read_prev
    }
    meter_clean$consumption[meter_clean$date == dates[i]] = cons
  }
}
# should probably have used a map function (they're more efficient)

# remove the first date (08-31)
meter_clean <- meter_clean %>% 
  filter(!(date == "2018-08-31"))



## Merge meter and WUnderground dataframes (part f)
source("Case2/scripts/WUnderground_clean.R")
WU_data <- combine

# Match the dates of the data frames
length(unique(meter_clean$date)) # 119 dates
length(unique(WU_data$date)) # 120 dates

sort(unique(meter_clean$date))
sort(unique(WU_data$date))

# differences:
# 2018-12-29 and 2018-12-30 not present in meter_clean
# 2018-09-17 not present in combine (WUnderground data)
meter_clean <- meter_clean %>% 
  filter(!(date == "2018-09-17"))

WU_data <- WU_data %>% 
  filter(!(date == "2018-12-29") & !(date == "2018-12-30"))

length(unique(meter_clean$date)) # 118 dates
length(unique(WU_data$date)) # 118 dates

# Remove "Reading" from the meter data
meter_clean <- meter_clean %>% 
  select(-Reading)

# Merge the data frames
merged_grp6 <- inner_join(meter_clean, WU_data, by = "date") %>% 
  select(date, everything())
merged_andreas <- fread("Case2/data/merged_data.csv")

# Summary of data frames
summary(merged_grp6)
summary(merged_andreas)
str(merged_grp6)
str(merged_andreas)
xtable(summary(merged_grp6[,1:6]))
xtable(summary(merged_grp6[,7:13]))
xtable(summary(merged_grp6[,9:13]))
summary(data)
xtable(summary(merged_andreas))
xtable(summary(data))

# Write csv file
# write_csv(merged_grp6, file = "Case2/data/merged_our_own.csv")
