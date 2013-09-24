probs <- dbinom(0:50, 50, 1/6)
plot(	0:50,probs,
	type = "h",
	xlab = "Number of fives rolled",
	ylab = "Probability of x fives",
	main = "dbinom(0:50,50,1/6)")