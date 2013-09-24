drug<-data.frame( 
	freq = rep(c(rep("1-9",2),rep("10-29",2),rep("Daily",2)),3),
	perceive = rep(c("actual","perceived"),9),
	drug = c(rep("Opiates",6), rep("Alcohol",6), rep("Cocaine",6)),
	p = c(0.7,29.4,0,2.4,0,0.4,56.9,30.9,21.1,56.3,1.4,11.2,0.3,48.4,0.2,5.3,0,1.2)/100
	)
drug$drug <- relevel(drug$drug, levels(drug$drug)[c(3,1,2)])


	
p <- ggplot(data = drug, aes(perceive, p))+
	geom_bar(aes(fill = perceive), position = "dodge")+
	facet_grid(drug~freq)+
	ylim(0,1)+
	scale_fill_manual(values = muted(c("blue","red")))+
	opts(legend.position = "none")
	
print(p)