par(mar = c(1,1,1,1))
plot(	x = 1:5,
	y = rep(2.5,5),
	ylim = c(1,3),
	cex = c(0.5,1,1.5,2,2.5),
	pch = 19,
	axes = F)
text(	x = 1:5,
	y = rep(1.5,5),
	labels = as.character(c(0.5,1,1.5,2,2.5)))
abline(h = 1:3,col = "grey")
abline(v = (1:5)+0.5,col = "grey")
box()
