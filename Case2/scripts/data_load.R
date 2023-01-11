library(tidyverse); library(car); library(ggpubr); library(data.table)

################################################################################
### Data loading and cleansing
################################################################################

## Read meter files (b+c)
# Initialise empty data frame
columns <- c("ID", "Time", "Reading")
meter <- data.frame(matrix(nrow = 0, ncol = length(columns)))

# Read meter files into data frame
files <- list.files("Case2/data/meterdata/")
for (filename in files){
  file <- fread(paste0("Case2/data/meterdata/", filename))
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

## Compute consumption for each building
# Initialise empty data frames
columns <- c("ID", "Time", "Reading")
temp.meter <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(temp.meter) <- columns

# Iterate over all meters
for (id in unique(meter_clean$ID)){
  # Define timepoints to interpolate to
  meter_current <- meter_clean %>% 
    filter(ID == id)
  timepoints <- data.frame("Time" = meter_current$Time) %>% 
    mutate(Time = as.POSIXct(paste0(substr(Time, 0, 10), " 23:59:00"), tz = "GMT"))
  timepoints <- timepoints[,1]
  
  # Interpolate data
  interpol.data <- approx(meter_current$Time, meter_current$Reading, xout = timepoints)
  # NAs generated 13 times (so for 13 buildings?) <-- look into this
  
  # Add data to temporary data frame
  temp.meter <- rbind(temp.meter,data.frame("ID" = id,
                                            "Time" = interpol.data$x,
                                            "Reading" = interpol.data$y))
}
# Update the data frame
meter_clean <- temp.meter
