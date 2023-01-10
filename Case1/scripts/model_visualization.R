source("Case1/scripts/final_model.R")

# Subsetting data
data_A <- data %>% 
  filter(Enzyme == "A")
data_B <- data %>% 
  filter(Enzyme == "B")
data_C <- data %>% 
  filter(Enzyme == "C")
data_D <- data %>% 
  filter(Enzyme == "D")
data_E <- data %>% 
  filter(Enzyme == "E")


# Creating new data
new.data_A <- data.frame("Enzyme" = rep(c("A"), each = 1000),
                         "DetStock" = rep(c("Det+", "Det0"), each = 500),
                         "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                         "new_EnzymeConc" = seq(min(data_A$new_EnzymeConc), max(data_A$new_EnzymeConc), length = 100))
new.data_B <- data.frame("Enzyme" = rep(c("B"), each = 1000),
                         "DetStock" = rep(c("Det+", "Det0"), each = 500),
                         "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                         "new_EnzymeConc" = seq(min(data$new_EnzymeConc), max(data$new_EnzymeConc), length = 100))
new.data_C <- data.frame("Enzyme" = rep(c("C"), each = 1000),
                         "DetStock" = rep(c("Det+", "Det0"), each = 500),
                         "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                         "new_EnzymeConc" = seq(min(data$new_EnzymeConc), max(data$new_EnzymeConc), length = 100))
new.data_D <- data.frame("Enzyme" = rep(c("D"), each = 1000),
                         "DetStock" = rep(c("Det+", "Det0"), each = 500),
                         "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
                         "new_EnzymeConc" = seq(min(data$new_EnzymeConc), max(data$new_EnzymeConc), length = 100))
new.data_E <- data.frame("Enzyme" = rep(c("E"), each = 1000),
                       "DetStock" = rep(c("Det+", "Det0"), each = 500),
                       "CaStock" = rep(c("Ca+", "Ca0"), each = 500),
"new_EnzymeConc" = seq(min(data$new_EnzymeConc), max(data$new_EnzymeConc), length = 100))

# Computing prediction intervals
pred.int_A <- predict(lm4.5, new.data_A, interval = "prediction")
pred.int_B <- predict(lm4.5, new.data_B, interval = "prediction")
pred.int_C <- predict(lm4.5, new.data_C, interval = "prediction")
pred.int_D <- predict(lm4.5, new.data_D, interval = "prediction")
pred.int_E <- predict(lm4.5, new.data_E, interval = "prediction")

# Plotting
par(mfrow=c(2,3))
# A
plot(new_response ~ new_EnzymeConc, data_A, col = as.numeric(data$DetStock),
     pch = 19, main = "Enzyme A", ylim = c(-5,40))
legend("topleft", legend = c("Det+", "Det0"),
       pch = 19, col = 1:length(unique(data$DetStock)))
matlines(new.data_A$new_EnzymeConc[new.data_A$DetStock=="Det+"], pred.int_A[new.data_A$DetStock=="Det+",2:3],
         lty = 3, col = 1, lw = 2)
matlines(new.data_A$new_EnzymeConc[new.data_A$DetStock=="Det+"], pred.int_A[new.data_A$DetStock=="Det+",1],
         lty = 1, col = 1, lw = 2)
matlines(new.data_A$new_EnzymeConc[new.data_A$DetStock=="Det0"], pred.int_A[new.data_A$DetStock=="Det0",2:3],
         lty = 3, col = 2, lw = 2)
matlines(new.data_A$new_EnzymeConc[new.data_A$DetStock=="Det0"], pred.int_A[new.data_A$DetStock=="Det0",1],
         lty = 1, col = 2, lw = 2)

# B
plot(new_response ~ new_EnzymeConc, data_B, col = as.numeric(data$DetStock),
     pch = 19, main = "Enzyme B", ylim = c(-5,40))
legend("topleft", legend = c("Det+", "Det0"),
       pch = 19, col = 1:length(unique(data$DetStock)))
matlines(new.data_B$new_EnzymeConc[new.data_B$DetStock=="Det+"], pred.int_B[new.data_B$DetStock=="Det+",2:3],
         lty = 3, col = 1, lw = 2)
matlines(new.data_B$new_EnzymeConc[new.data_B$DetStock=="Det+"], pred.int_B[new.data_B$DetStock=="Det+",1],
         lty = 1, col = 1, lw = 2)
matlines(new.data_B$new_EnzymeConc[new.data_B$DetStock=="Det0"], pred.int_B[new.data_B$DetStock=="Det0",2:3],
         lty = 3, col = 2, lw = 2)
matlines(new.data_B$new_EnzymeConc[new.data_B$DetStock=="Det0"], pred.int_B[new.data_B$DetStock=="Det0",1],
         lty = 1, col = 2, lw = 2)

# C
plot(new_response ~ new_EnzymeConc, data_C, col = as.numeric(data$DetStock),
     pch = 19, main = "Enzyme C", ylim = c(-5,40))
legend("topleft", legend = c("Det+", "Det0"),
       pch = 19, col = 1:length(unique(data$DetStock)))
matlines(new.data_C$new_EnzymeConc[new.data_C$DetStock=="Det+"], pred.int_C[new.data_C$DetStock=="Det+",2:3],
         lty = 3, col = 1, lw = 2)
matlines(new.data_C$new_EnzymeConc[new.data_C$DetStock=="Det+"], pred.int_C[new.data_C$DetStock=="Det+",1],
         lty = 1, col = 1, lw = 2)
matlines(new.data_C$new_EnzymeConc[new.data_C$DetStock=="Det0"], pred.int_C[new.data_C$DetStock=="Det0",2:3],
         lty = 3, col = 2, lw = 2)
matlines(new.data_C$new_EnzymeConc[new.data_C$DetStock=="Det0"], pred.int_C[new.data_C$DetStock=="Det0",1],
         lty = 1, col = 2, lw = 2)

# D
plot(new_response ~ new_EnzymeConc, data_D, col = as.numeric(data$DetStock),
     pch = 19, main = "Enzyme D", ylim = c(-5,40))
legend("topleft", legend = c("Det+", "Det0"),
       pch = 19, col = 1:length(unique(data$DetStock)))
matlines(new.data_D$new_EnzymeConc[new.data_D$DetStock=="Det+"], pred.int_D[new.data_D$DetStock=="Det+",2:3],
         lty = 3, col = 1, lw = 2)
matlines(new.data_D$new_EnzymeConc[new.data_D$DetStock=="Det+"], pred.int_D[new.data_D$DetStock=="Det+",1],
         lty = 1, col = 1, lw = 2)
matlines(new.data_D$new_EnzymeConc[new.data_D$DetStock=="Det0"], pred.int_D[new.data_D$DetStock=="Det0",2:3],
         lty = 3, col = 2, lw = 2)
matlines(new.data_D$new_EnzymeConc[new.data_D$DetStock=="Det0"], pred.int_D[new.data_D$DetStock=="Det0",1],
         lty = 1, col = 2, lw = 2)

# E
plot(new_response ~ new_EnzymeConc, data_E, col = as.numeric(data$DetStock),
     pch = 19, main = "Enzyme E", ylim = c(-5,40))
legend("topleft", legend = c("Det+", "Det0"),
       pch = 19, col = 1:length(unique(data$DetStock)))
matlines(new.data_E$new_EnzymeConc[new.data_E$DetStock=="Det+"], pred.int_E[new.data_E$DetStock=="Det+",2:3],
         lty = 3, col = 1, lw = 2)
matlines(new.data_E$new_EnzymeConc[new.data_E$DetStock=="Det+"], pred.int_E[new.data_E$DetStock=="Det+",1],
         lty = 1, col = 1, lw = 2)
matlines(new.data_E$new_EnzymeConc[new.data_E$DetStock=="Det0"], pred.int_E[new.data_E$DetStock=="Det0",2:3],
         lty = 3, col = 2, lw = 2)
matlines(new.data_E$new_EnzymeConc[new.data_E$DetStock=="Det0"], pred.int_E[new.data_E$DetStock=="Det0",1],
         lty = 1, col = 2, lw = 2)