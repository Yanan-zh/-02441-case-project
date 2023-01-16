# make sure to have final_model from analysis

summary(final_model)

Anova(final_model)


glm(final_model)


# conf int ----------------------------------------------------------------

# we make a 83 by 285



#frist 83

A_1 <- matrix(0,83,83)

#83
#for tempDiff

A_2 <- matrix(1,83,1)

#84
#for weekendTrue, windspeed

A_3 <- matrix(0,83,2)

#86

#for direction

A_4 <- matrix(0,83,15)

# 101
# for humiditiy

A_5 <- matrix(0,83,1)

# 102

# for interaction id:tempDIFF

A_6_top <-matrix(0,1,82)

A_6_bot <-matrix(diag(82),82,82)

A_6 <- rbind(A_6_top,A_6_bot)

# 184

# for weekend interaction

A_7_top <-matrix(0,1,82)

A_7_bot <-matrix(diag(82),82,82)

A_7 <- rbind(A_7_top,A_7_bot)

# 266

# for non id interactions weekend:windspeed, tempDiff:windspeed

A_8 <- matrix(0,83,3)

# 269 

# for dir:wind 

A_9 <- matrix(0,83,15)

# 284
#for windspd:hum

A_10 <- matrix(0,83,1)

#285

# design matrix

model_sum <- summary(final_model,correlation = TRUE)

A <- cbind(A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8,A_9,A_10)

est <- A%*%model_sum$coefficients[,1]

var_est <-A%*%model_sum$cov.unscaled%*% t(A)*model_sum$sigma^2

coef <-data.frame(Group=levels(data$ID), Slope = est, sd.error=sqrt(diag(var_est)))
