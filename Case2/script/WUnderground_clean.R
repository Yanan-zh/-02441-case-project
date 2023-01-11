install.packages('rstatix')
library(rstatix)
library(stringr)


#load data
#-------------------------------------

load('Case2/WUndergroundHourly.RData')

# data clean
#-------------------------------------

#remove pure NA colunm
str(WG)
summary(WG)
WG_clean = WG[,-c(6,10,11,12,13,14,19,20,21)]
summary(WG_clean)

# factor some of variables
factor <- c("dir","cond","fog","rain","snow")

WG_clean[factor] <- lapply(WG_clean[factor], factor)  ## as.factor() could also be used
summary(WG_clean)

# seperate date colunm
WG_clean[c('day', 'time')] = str_split_fixed(WG_clean$date, " ", 2)

# handle missing values
 
# fix pressure (1000)


WG_clean[is.na(WG_clean$pressure),][,1]

pressure_miss_day = WG_clean[WG_clean$day== '2018-12-21',][-13,]
plot(pressure_miss_day$pressure)

date.inter <- as.POSIXct(WG_clean[is.na(WG_clean$pressure),][,1])
inter.result <- approx(pressure_miss_day$date, pressure_miss_day$pressure, xout = date.inter)

WG_clean['pressure'][is.na(WG_clean['pressure'])]= inter.result$y


summary(WG_clean)
str(WG_clean)

#_______________________________________________
# caculate per day

# make a function caculate mode of factor
mode = function(x){
  names(which.max(table(x)))
}


# initial prepare of for loop
number = data.frame(unique(WG_clean$day)) 
colnames(number) = c('unique.WG_clean.day.'='date')
factors = data_frame(unique(WG_clean$day))
colnames(factors) = c('unique.WG_clean.day.'='date')


# loop go through all colunm and combine the result
for (i in seq(2,length(WG_clean[1,])-2)){
  if (is.factor(WG_clean[,i]) == FALSE){
    number_ = aggregate(WG_clean[,i], list(date=WG_clean$day), function(x) mean(x,na.rm = TRUE))
    colnames(number_) = c('date',names(WG_clean)[i])
    number = cbind(number,number_[-1])
  }
  if (is.factor(WG_clean[,i]) == TRUE){
    WG_clean_factor = WG_clean[WG_clean[,i]!='',]
    factors_ = aggregate(WG_clean_factor[,i], list(date=WG_clean_factor$day), mode)
    colnames(factors_) = c('date', names(WG_clean)[i])
   factors = cbind(factors, factors_[-1])
  }
}

# combine result
combine = data_frame()
combine = cbind(number,factors[-1])

#------------------------------------------------
# inspect data again 
summary(combine)

# all snow rows with 0 
combine[combine$snow != '0']

# delete snow as well 
combine = combine[-12]
summary(combine)





#below not finished 
########################################
# missing vis? handel it? not done 


WG_clean[WG_clean$day== '2018-11-12',]$cond

plot(WG_clean[WG_clean$day== '2018-10-04',]$vis)


vis_miss_day_ = WG_clean[WG_clean$day== '2018-11-10',][-16,]
date.inter <- as.POSIXct("2018-12-23 23:00:00")
inter.result <- approx(vis_miss_day_$date, vis_miss_day_$vis, xout = date.inter)

WG_clean[is.na(WG_clean$vis),][,1]

#plot(vis_miss_day_$vis~vis_miss_day_$date)
#points(inter.result$x, inter.result$y, pch = 17)
#legend("topleft", legend = c("data", "interpolated"), pch = c(1,17))

vis_miss_day = WG_clean[is.na(WG_clean$vis),]


for (i in c(1:length(vis_miss_day[,1]))){
  vis_miss_day_w = WG_clean[WG_clean$day == vis_miss_day[i,16],]
  vis_miss_day_n = na.omit(vis_miss_day_w)
  date.inter <- as.POSIXct(vis_miss_day[i,1])
  inter.result <- approx(vis_miss_day_n$date, vis_miss_day_n$vis, xout = date.inter)
  print(inter.result$y)
  }

#########################################################










