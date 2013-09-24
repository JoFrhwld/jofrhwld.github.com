pred<-expand.grid(Log_Freq = seq(min(sbuck$Log_Freq),max(sbuck$Log_Freq),length = 50),Gram = levels(sbuck$Gram))
pred$fit<-predict(bmod2,newdata = pred,type = "response")

plot(sbuck$Log_Freq,sbuck$DepVar, type = "n")
library(RColorBrewer)
gpal <- brewer.pal(4,"Set1")
gs<-levels(pred$Gram)

for(i in 1:4){
	g<-gs[i]
	gpred<- subset(pred,Gram == g)
	lines(gpred$Log_Freq,gpred$fit,col = gpal[i],lwd = 2)
}
legend("topright",legend = gs,col = gpal,lwd = 2)