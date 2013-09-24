p <- ggplot(data = gender.comp, aes(Male, Female))+
	geom_abline(colour = "grey80")+
	geom_point(alpha = 0.6)+
	facet_wrap(~Measure, scales = "free")+
	opts(aspect.ratio = 1)
	
print(p)