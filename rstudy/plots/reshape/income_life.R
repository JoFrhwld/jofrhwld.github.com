p <- ggplot(subset(gender.comp2,Income < 40000), aes(Income, LifeExp,colour = Gender))+
	scale_x_log10()+
	geom_point()+
	stat_smooth(method = "lm")+
	opts(aspect.ratio = 2/(1+sqrt(5)))

print(p)