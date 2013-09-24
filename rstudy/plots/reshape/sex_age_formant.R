p <- ggplot(pb.means1, aes(variable, `(all)`))+
	geom_line(aes(color = Sex, linetype = Age, group = Sex:Age))+
	facet_wrap(~Vowel)
		
print(p)