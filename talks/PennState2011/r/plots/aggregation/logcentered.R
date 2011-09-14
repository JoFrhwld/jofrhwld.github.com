p <- ggplot(dur, aes(Begin, Dur_center))+
	geom_point()+
	geom_hline(y = 0)+
	stat_smooth()
print(p)