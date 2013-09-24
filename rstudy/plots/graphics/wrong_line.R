p <- ggplot(data, aes(Groups, Measure))+
	geom_point()+
	geom_line(aes(group = 1))+
	expand_limits(y = 0)
	
print(p)