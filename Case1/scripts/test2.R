# test script
library(tidyverse); library(ggpubr); library(car)

# Data loading
data <- read.csv("Case1/scripts/SPR.txt", sep="")
data$Enzyme = as.factor(data$Enzyme)
data$DetStock = as.factor(data$DetStock)
data$CaStock = as.factor(data$CaStock)
data$sqrtResponse = sqrt(data$Response)
str(data)


# Visualisation
normal_plot <- function(col){
  par(mfrow=c(1,3))
  x <- c(unlist(data[col]))
  hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Histogram of", col))
  x_range <- seq(min(x), max(x), length.out = 100)
  lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
  plot(ecdf(x), main = paste0("ecdf(", col, ")"))
  lines(x_range, pnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
  abline(v=hist1$breaks)
  qqnorm(x); qqline(x)
}
normal_plot("sqrtResponse")
shapiro.test(data$sqrtResponse)
# still not normal


# Histogram of Response for dif. CaStock
par(mfrow=c(1,2))
x <- data$Response[data$CaStock == "Ca+"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Ca+"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
x <- data$Response[data$CaStock == "Ca0"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Ca0"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")

# Histogram of Response for dif. DetStock
par(mfrow=c(1,2))
x <- data$Response[data$DetStock == "Det+"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Det+"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
x <- data$Response[data$DetStock == "Det0"]
hist1 <- hist(x, freq = FALSE, breaks = 8, main = paste("Response for Det0"))
x_range <- seq(min(x), max(x), length.out = 100)
lines(x_range, dnorm(x_range, mean(x), sd(x)), lw = 3, col = "red")
shapiro.test(data$Response[data$DetStock == "Det+"])


## Box plots
# Detergent
par(mfrow=c(1,1))
data %>%
  ggplot(mapping = aes(x = DetStock,
                       y = Response,
                       fill = DetStock)) +
  geom_boxplot() +
  theme_minimal()
# The presence of a detergent enhances the catalytic activity

# Calcium
data %>%
  ggplot(mapping = aes(x = CaStock,
                       y = Response,
                       fill = CaStock)) +
  geom_boxplot() +
  theme_minimal()

# Enzyme type
data %>%
  filter(CaStock == "Ca0") %>% 
  ggplot(mapping = aes(x = Enzyme,
                       y = Response,
                       fill = Enzyme)) +
  geom_boxplot() +
  theme_minimal() +
  geom_pwc()

# Enzyme type by Enzyme Conc. + KW test
data %>%
  ggplot(mapping = aes(x = Enzyme,
                       y = Response,
                       fill = as.factor(EnzymeConc))) +
  geom_boxplot() +
  theme_classic() +
  stat_compare_means(label.y = c(1588+20,1000.5+20,1133.7+20,742.1+20,1162.7+20)) +
  labs(fill = "Enzyme concentration",
       x = "Enzyme type") +
  ylim(0,1610)
ggsave("Case1/figures/boxplot_enz_conc_kruskal.png", dpi = 300, width = 3200, height = 1600, unit = "px")
max(data$Response[data$Enzyme == "E"])
