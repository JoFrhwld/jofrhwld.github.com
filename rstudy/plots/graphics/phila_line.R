phil.comp <- cast(phil, Name + Level ~ Gender)


p <- ggplot(phil.comp, aes(Male, Female))+
	geom_abline(colour = "grey")+
	geom_point(aes(colour = Name, shape = Level),size = 2.5)+
	geom_path(aes(colour = Name))+
	coord_equal()+
	expand_limits(x = c(min(phil.comp$Female), max(phil.comp$Male)),
		y = c(min(phil.comp$Female), max(phil.comp$Male)))+
	scale_y_log10()+
	scale_x_log10()
	
print(p)

