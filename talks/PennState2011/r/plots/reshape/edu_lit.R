 p <- ggplot(subset(gender.comp2,Edu < 99 &  Literacy < 99), aes(Edu/100, Literacy/100,colour = Gender))+
 	scale_x_logit()+
 	scale_y_logit()+
 	geom_point()+
 	stat_smooth()+
 	opts(aspect.ratio = 2/(1+sqrt(5)))
 
 print(p)