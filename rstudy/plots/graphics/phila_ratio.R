phil.comp$Ratio <- phil.comp$Female / phil.comp$Male

p <- ggplot(phil.comp, aes(Level, Ratio))+
	geom_line(aes(color = Name, group = Name),size = 2)+
	ylim(0.5,1)
	
print(p)