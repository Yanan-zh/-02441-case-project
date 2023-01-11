calw = SPR[SPR$CaStock=='Ca+',][3]
caln = SPR[SPR$CaStock=='Ca0',][3]

str(calw)

detw = SPR[SPR$DetStock=='Det+',][3]
detn = SPR[SPR$DetStock=='Det0',][3]

shapiro.test(sqrt(detw$Response))
shapiro.test(sqrt(detn$Response))

png(filename="pairsplot.png",res=130) # perhaps width/height as well
# ...
pairs(SPR[-1], pch=as.numeric(SPR$DetStock)+15, col=SPR$EnzymeConc, diag.panel=panel.hist, lower.panel = panel.smooth)
dev.off()

ggplot()+
geom_point(data = SPR,mapping =aes(sort(unique(EnzymeConc))))


ggplot(data = SPR,mapping =aes(Enzyme, Response, fill=as.factor(DetStock)))+
  geom_boxplot()+ guides(fill=guide_legend(title="DetStock"))+ theme_classic()


ggplot(data = SPR,mapping =aes(Enzyme, Response, fill=as.factor(CaStock)))+
  geom_boxplot()+ guides(fill=guide_legend(title="Catstock"))+ theme_classic()


plot(sort(unique(SPR$EnzymeConc)))
par(new=TRUE)


plot(SPR$EnzymeConc ~ sqrt(SPR$Response),col="green")


plot(sqrt(SPR$EnzymeConc) ~ sqrt(SPR$Response),col="yellow")


plot((SPR$EnzymeConc)^2~sqrt(SPR$Response), col="red" )

plot(log(SPR$EnzymeConc)~sqrt(SPR$Response), col="red" )



par(new=TRUE)
plot(sort(unique((SPR$EnzymeConc))), type="l", col="red" )







str(data)



library(tree)
tree1 <- tree(sqrt(Response) ~ ., data)
par(mfrow = c(1,1))
plot(tree1)
text(tree1)

coplot(sqrt(Response) ~ EnzymeConc|DetStock , data, panel = panel.smooth)





ggplot(data = data,mapping= aes(DetStock,Response,fill=DetStock))+
  geom_boxplot()+ theme_classic()+
  stat_compare_means()


ggplot(data = data,mapping =aes(CaStock ,Response,fill=CaStock))+
  geom_boxplot()+theme_classic()+ stat_compare_means()

ggplot(data = data,mapping =aes(Enzyme, Response, fill=as.factor( EnzymeConc)))+
  geom_boxplot()+ guides(fill=guide_legend(title="EnzymeConc"))+ theme_classic()+ stat_compare_means()



