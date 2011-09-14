p <- ggplot(subset(gender.comp2,Edu < 99 & Income < 40000), aes(Edu/100, Income,colour = Gender))+
	scale_x_logit()+
	scale_y_log10()+
	geom_point()+
	stat_smooth(method = "lm")+
	opts(aspect.ratio = 2/(1+sqrt(5)))
 
 print(p)