par(mar = c(1,1,1,1))
plot(	x = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),
	y = rep(5:1,5),
	axes = F,
	xlab = "",ylab = "",
	xlim = c(1,6),ylim = c(1,5),
	pch = 1:25,
	bg = "red",cex = 3)
text(	x = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))+.5,
	y = rep(5:1,5),
	labels = as.character(1:25))
abline(h = (1:5)+0.5,col = "grey")
abline(v = (1:5)+0.75,col = "grey")