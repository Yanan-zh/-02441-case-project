source("Case1/scripts/final_model.R")

# Plotting function
sub_plot <- function(){
  par(mfrow=c(2,3))
  for (enzyme in sort(unique(data$Enzyme))){
    data_sub <- data %>% 
      filter(Enzyme == enzyme)
    
    new.data <- data.frame("Enzyme" = rep(c(enzyme), each = 1000),
                           "DetStock" = rep(c("Det+", "Det0"), each = 500),
                           "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                           "new_EnzymeConc" = seq(min(data_sub$new_EnzymeConc), max(data_sub$new_EnzymeConc), length = 100))
    
    pred.int <- predict(lm4.5, new.data, interval = "prediction")
    
    plot(new_response ~ new_EnzymeConc, data_sub, col = as.numeric(data_sub$DetStock),
         pch = 19, main = paste("Enzyme", enzyme), ylim = c(-2,42),
         xlab = "Enzyme concentration", ylab = "Response")
    legend("topleft", legend = c("Detergent present", "Detergent absent"),
           pch = 19, col = 1:length(unique(data_sub$DetStock)))
    matlines(new.data$new_EnzymeConc[new.data$DetStock=="Det+"], pred.int[new.data$DetStock=="Det+",],
             lty = c(1,3,3), col = 1, lw = 2)
    matlines(new.data$new_EnzymeConc[new.data$DetStock=="Det0"], pred.int[new.data$DetStock=="Det0",],
             lty = c(1,33), col = 2, lw = 2)
  }
  par(mfrow=c(1,1))
}

sub_backtransform <- function(){
  par(mfrow=c(2,3))
  for (enzyme in sort(unique(data$Enzyme))){
    data_sub <- data %>% 
      filter(Enzyme == enzyme)
    
    new.data <- data.frame("Enzyme" = rep(c(enzyme), each = 1000),
                           "DetStock" = rep(c("Det+", "Det0"), each = 500),
                           "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                           "new_EnzymeConc" = seq(min(data_sub$new_EnzymeConc), max(data_sub$new_EnzymeConc), length = 500))
    new.data$EnzymeConc = new.data$new_EnzymeConc * new.data$new_EnzymeConc
    pred.int <- predict(lm4.5, new.data, interval = "predictions")
    pred.int <- pred.int^2
    # sort.fit <- pred.int[,1] %>% sort()
    
    plot(Response ~ EnzymeConc, data_sub, col = as.numeric(data_sub$DetStock),
         pch = 19, main = paste("Enzyme", enzyme), ylim = c(0,1600),
         xlab = "Enzyme concentration", ylab = "Response")
    legend("topleft", legend = c("Detergent present", "Detergent absent"),
           pch = 19, col = 1:length(unique(data_sub$DetStock)))
    matlines(new.data$EnzymeConc[new.data$DetStock=="Det+"], pred.int[new.data$DetStock=="Det+",],
             lty = c(1,3,3), col = 1, lw = 2)
    matlines(new.data$EnzymeConc[new.data$DetStock=="Det0"], pred.int[new.data$DetStock=="Det0",],
             lty = c(1,3,3), col = 2, lw = 2)
  }
}

# Plotting
# sub_plot()
png(file = "Case1/figures/model_visualization.png", width = 2000, height = 1600, pointsize = 40)
sub_backtransform()
dev.off()
