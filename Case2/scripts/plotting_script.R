source("Case2/scripts/plot_functions.R")
library(tidyverse); library(car); library(ggpubr); library(ggrepel)

################################################################################
### Plot tests
################################################################################

p1 <- scatter_plot(ran=c(81,83), label_ = "ID")
p2 <- scatter_plot(ran=c(81,83), label_ = "ID", x_ = "wind_spd")
ggarrange(p1,p2)

scatter_plot(ran=c(65,75), col = "dir")

cor(data$wind_spd, as.numeric(data$dir))


################################################################################
### Outliers
################################################################################
# Outliers (ID, date)
# 4962433,  2018-12-09
# 5140250,  2018-12-12
# 

# 65012411, 2018-10-30
# 65012411, 2018-11-06
# 65012411, 2018-11-08
# 65012411, x

# 65118755:
# Seems like it has three different groupings/slopes --> gutebei!

# 65118764:
# similar to 65118755

# 65118848
# quite a few outliers

# 65067046, 2018-11-14

# 65118805
# 6790785,  2018-11-20
# 6790785,  2018-11-22
# 6790798,  2018-11-13

# 69652588
# one group with no slope and another with a slope

# 69749518
# like 69652588

# 7072241, 2018-09-23
# 7072241, 2018-12-05
# 7072241, 2018-12-07