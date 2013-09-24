p <- ggplot(casualties.comp, aes(Iraq, Afghanistan))+
	geom_abline(colour = "grey80")+
	geom_point()+
	scale_x_log10()+
	scale_y_log10()+
	facet_wrap(~Type)
	
print(p)