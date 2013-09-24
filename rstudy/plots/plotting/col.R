par(mar = c(1,1,1,1))
barplot(rep(1,8),col = 1:8,space = 0,axes = F)
text((1:4)-0.5,rep(0.5,4),as.character(1:4),col = "white")
text((5:8)-0.5,rep(0.5,4),as.character(5:8),col = "black")