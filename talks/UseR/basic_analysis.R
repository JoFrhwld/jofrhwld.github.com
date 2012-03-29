library(ggplot2)
library(reshape2)
library(plyr)
library(mgcv)


ndays <- data.frame(month.date = seq(as.Date("1988-01-01"), 
																		 as.Date("2011-12-01"), by = "month"),
					 ndays = as.numeric(diff(seq(as.Date("1988-01-01"), 
					 														as.Date("2012-01-01"), by = "month"))))

philly <- join(philly, ndays, type="left")


philly.bw <- subset(philly, race %in% c("B","W"))



months <- expand.grid(month.date = unique(philly.bw$month.date),
											race = c("B","W","H"),
											weapon = levels(philly.bw$weapon))
philly.bw.count.w <- count(philly.bw, c("month.date","month","race","ndays", "weapon"))
philly.bw.count.w <- subset(philly.bw.count.w, !is.na(month.date) & 
																								weapon %in% c("FIREARM", 
																															"KNIFE",
																															"HANDS"))

p1 <- ggplot(philly.bw.count.w, aes(month.date, freq, color = weapon)) + 
		geom_point()+
		stat_smooth()+
		facet_wrap(~race, scale = "free_y")+
		scale_color_hue(limits = c("FIREARM","KNIFE","HANDS"))

w.mod <- glm(freq ~ month * race * weapon, 
						 offset = ndays, 
						 family = "poisson", 
						 data = philly.bw.count.w)

w.mod2 <- glm(freq ~ month + race * weapon, 
						 offset = ndays, 
						 family = "poisson", 
						 data = philly.bw.count.w)

philly.bw.count.w$resid <- resid(w.mod2)

philly.bw.count <- count(philly.bw, c("month.date","month", "race","ndays"))
philly.bw.count <- subset(philly.bw.count, !is.na(month.date))


p2 <- ggplot(philly.bw.count, aes(month.date, 1/(freq/ndays), color = race)) + 
	geom_point()+
	scale_color_hue(limits = c("B","W"))+
	stat_smooth()+
	coord_trans(ytrans="log2")+
	scale_y_continuous(breaks = c(0.5,1,2,7,14,21,31))+
	expand_limits(y = 0.5)+
	ylab("1 murder every X days")+
	theme_bw()


philly.bw.date <- subset(philly.bw, !is.na(month))

mod <- glm(freq ~ month*race, family = "poisson", data = philly.bw.count,offset=ndays)
philly.bw.count$resid <- resid(mod)




