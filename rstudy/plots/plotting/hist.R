par(mfrow = c(1,3))
hist(energy$expend,main = "Default",col = "darkgrey")
hist(energy$expend,breaks = 10,main = "breaks = 10",col = "darkgrey")
hist(energy$expend,breaks = c(6.13,7,7.5,10,11,12.79),main = "breaks = c(6.13,7,7.5,10,11,12.79)",col = "darkgrey")