p <- ggplot(subset(gender.comp2,Edu < 99 & Income < 40000), aes(Edu/100, Income,colour = Gender))+
	scale_x_logit()+
	scale_y_log10()+
	geom_point()+
	geom_line(aes(group = Country),colour = "black", alpha = 0.65)+
	opts(aspect.ratio = 2/(1+sqrt(5)))
 
 print(p)