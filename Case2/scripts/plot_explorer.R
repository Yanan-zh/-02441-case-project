source("Case2/scripts/plot_functions.R")
library(RColorBrewer)

# Create single plots for all IDs
for (id in ids){
  scat_plot(id, col = "weekend", shape_ = "building_type", label_ = "date")
  ggsave(paste0("Case2/figures/explorative_plots/", id, "_scatter_weekend.png"),
         dpi = 300, width = 2000, height = 1600, units = "px")
}



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
  ggtitle("Consumption is lower during the weekend")



# Candidates for no weekend correlation:
# 4887707, long-term housing
# 4962433, fitness gym
# 5037175, long-term housing

p2 <- scat_plot(c(4887707
            ), col = "weekend", shape_ = "building_type") + 
  scale_colour_discrete("Weekend") +
  scale_shape("Building type") +
  ggtitle("Consumption independent of weekend status")

p3 <- scat_plot(c(4962433
), col = "weekend", shape_ = "building_type") + 
  scale_colour_discrete("Weekend") +
  scale_shape("Building type") +
  ggtitle("Consumption independent of weekend status")



# Candidates for off-season trend:
# 6392172, day care <- dates: 10-25, 10-29, 11-15, 11-22, 11-27
# 65118848, youth centre <- indoor tennis gym, dates = early dates

p4 <- scat_plot(c(65118848),
          col_ = "date", shape_ = "building_type") +
  scale_shape("Building type") +
  ggtitle("Blåkilde IF Tennis")



# Candidates for three slopes
# 65118755, apartment <- unknown trend
# 65118764, day care private <- unknown trend
p5 <- scat_plot(65118755, shape_ = "building_type", col_ = "date") +
  scale_shape("Building type") +
  ggtitle("Outlier patterns")



# Candidates for no correlation between consumption and temperature difference:
# 69999051, after school care
p6 <- scat_plot(69999051, shape_ = "building_type", col_ = "date") +
  ggtitle("Date-dependent distributions")



# Final plot
ggarrange(p1,p2,p3,p5,p4,p6)
ggsave(paste0("Case2/figures/scatter_weekend_6plts.png"),
       dpi = 300, width = 4000, height = 2400, units = "px")
