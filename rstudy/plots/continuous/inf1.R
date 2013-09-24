INF1 <- read.delim("http://www.ling.upenn.edu/~joseff/rstudy/data/INF1.txt")
cor.INF1 <- cor(INF1, use = "pairwise.complete.obs")
image(	abs(cor.INF1),
	axes = F,
	col = grey((2:0)/2)
)
axis(	1,
	labels = colnames(INF1),
	at = seq(0,1,length = ncol(INF1)),
	las = 2
)
axis(	2,
	labels = colnames(INF1),
	at = seq(0,1,length = ncol(INF1)),
	las = 1
)