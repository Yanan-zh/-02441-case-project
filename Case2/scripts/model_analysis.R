# make sure to have final_model from analysis

summary(final_model)

Anova(final_model)


glm(final_model)


# conf int ----------------------------------------------------------------

# we make a 83 by 285



#frist 83

A_1 <- matrix(0,83,83)

#for tempDiff

A_2 <- matrix(1,83,1)


#for weekendTrue, windspeed

A_3 <- matrix(0,83,2)


#for direction

A_4 <- matrix(0,83,15)

# for humiditiy

A_5 <- matrix(0,83,1)

# for interaction id:tempDIFF

A_6 <- diag(83)

# for weekend interaction

A_7 <- diag(83)

# for non id interactions weekend:windspeed, tempDiff:wind, tempDiff:weekend

A_8 <- matrix(0,83,3)

# for dir:wind 

A_8 <- matrix(0,83,15)

#for windspd:hum

A_9 <- matrix(0,83,1)

# design matrix

A <- cbind(A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8,A_9)