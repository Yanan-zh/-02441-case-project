
data <- read.csv("Case1/SPR.txt", sep="")
data = data [ data$EnzymeConc== 0,]

data = data[-c(40),]
data$RunDate = as.factor (data$RunDate)


lm5 = lm(Response~RunDate*Cycle*DetStock,data)

bc2 = boxCox(lm5, lambda = seq(0,0.5,0.05))
lambda = bc2$x[ which.max(bc2$y) ]
data$Response1 = (data$Response ^ lambda - 1) / lambda



lm5.1 <- lm(data$Response1~RunDate*Cycle*DetStock,data)
step(lm5.1)

lm5.2 = lm(formula = data$Response1 ~ RunDate + Cycle + DetStock + Cycle:DetStock, 
   data = data)

drop1(lm5.2,test="F")
lm5.3<- update(lm5.2, . ~ . -Cycle:DetStock)
drop1(lm5.3,test="F")

lm5.4<- update(lm5.3, . ~ . -Cycle)
drop1(lm5.4,test="F")


png("error.png", units = "in", width = 10, height = 7, res = 300)


par(mfrow=c(2,2))
plot(lm5.4, col=as.numeric(data$RunDate)+7, pch=19)
dev.off()


summary(lm5.4)
shapiro.test(lm5.4$residuals)



