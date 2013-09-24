p <- ggplot(data, aes(Groups, Measure))+
	geom_bar(aes(fill = Groups), color = "black")+
	geom_text(aes(y = Measure + 0.15, label = Measure))

print(p)