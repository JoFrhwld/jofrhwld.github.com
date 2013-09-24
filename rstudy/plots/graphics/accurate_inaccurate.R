data <- data.frame(Groups = c("A","B"), Measure = c(2,5))


p1 <- ggplot(data = data, aes(Groups, Measure))+
	geom_point(size = 5)+
	geom_segment(aes(xend = Groups), yend = 0, size = 3)+
	ylim(0,5)+
	opts(title = "Accurate")
print(p1)


p2 <- ggplot(data = data, aes(Groups, Measure))+
	geom_point(size = 5)+
	geom_segment(aes(xend = Groups), yend = 1.75, size = 3)+
	ylim(1.75,5)+
	opts(title = "Inaccurate")
print(p2)

