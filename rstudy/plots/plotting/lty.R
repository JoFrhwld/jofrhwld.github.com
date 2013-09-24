par(mar = c(1,1,1,1))
a<-c(1,0,1,1)
plot(	1,1,
	type = "n",
	xlim = c(1,4),ylim = c(0,6),
	axes = F,
	xlab = "",ylab = "")
matlines(	x = 1:4,
	y = cbind(a,a+1,a+2,a+3,a+4,a+5),
	col = "black",
	lty = 1:6,
	lwd = 2)
text(	x = rep(2,6),
	y = (1:6)-0.75,
	labels = as.character(1:6))
box()
