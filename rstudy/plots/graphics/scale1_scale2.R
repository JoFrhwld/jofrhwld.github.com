p1 <- ggplot(data, aes(Groups, Measure))+
	geom_bar()
print(p1)

p2 <- ggplot(data, aes(Groups, Measure))+
	geom_bar()+
	expand_limits(y = 20)
print(p2)