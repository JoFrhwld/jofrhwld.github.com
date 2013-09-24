par(mfrow=c(2,2),mex = 0.75)

for(i in c(5,35,70,100)){
	means<-vector()
	for(j in 1:1000){
		means[j]<-mean(rnorm(i))
	}
	ymax <- dnorm(mean(means),mean(means),sd(means))
	curve(dnorm(x),-2,2,ylim = c(0,ymax))
	hist(means,add = T,prob = T)
	curve(dnorm(x,mean(means),sd(means)),add = T,col = "red")
	title(paste("n = ",i))
}