pred1<-expand.grid(Log_Freq = seq(min(sbuck$Log_Freq),max(sbuck$Log_Freq),length = 50),Gram = levels(puck$Gram),Gram2 = "other")
pred2<-expand.grid(Log_Freq = seq(min(sbuck$Log_Freq),max(sbuck$Log_Freq),length = 50),Gram = "Past",Gram2 = "past")
pred<-rbind(pred2,pred1)
pred$fit<-predict(bmod4,newdata = pred,type = "response")
pred$se<-predict(bmod4,newdata = pred,type = "response",se = T)$se.fit


plot(sbuck$Log_Freq,sbuck$DepVar, type = "n")
library(RColorBrewer)
gpal <- brewer.pal(4,"Set1")
gs<-levels(pred$Gram)

for(i in 1:4){
	g<-gs[i]
	gpred<- subset(pred,Gram == g)
	grange<-range(sbuck[sbuck$Gram == g,]$Log_Freq)
	gpred<-gpred[gpred$Log_Freq<=grange[2]&gpred$Log_Freq >= grange[1],]
	
	
	ses<-cbind(gpred$fit,gpred$fit+gpred$se,gpred$fit-gpred$se)
	matlines(gpred$Log_Freq,ses,col = gpal[i],lwd = 2,lty = c(1,3,3))
}
legend("topright",legend = gs,col = gpal,lwd = 2)