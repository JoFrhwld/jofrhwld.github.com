segs<-rbind(	c(-2,-1.5),
	c(-1.5,-1),
	c(-1,-0.5),
	c(-0.5,0),
	c(0,0.5),
	c(0.5,1),
	c(1,1.5),
	c(1.5,2))
pals <- c("YlOrRd","Blues","Set1","RdYlBu")
m<-matrix(1:length(pals),ncol = 2)
layout(m)
par(mar = c(1,1,1,1))
for(j in pals){
	curve(dnorm(x),-2,2,axes = F)
	pal<-brewer.pal(nrow(segs),j)
	for(i in 1:nrow(segs)){
		seg<-segs[i,]
		top<-dnorm(seq(seg[1],seg[2],0.05))
		xx<-seq(seg[1],seg[2],0.05)
		polygon(c(seg[1],xx,seg[2]),c(0,top,0),col = pal[i] )
	
	
	}
}