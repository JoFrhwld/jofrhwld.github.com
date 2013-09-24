p <- ggplot(casualties.comp2, aes(HOSTILE.DEAD, NONHOSTILE.DEAD))+
	geom_abline(colour = "grey80")+
	geom_point()+
	scale_x_log10()+
	scale_y_log10()+
	facet_wrap(~Country)
	
print(p)