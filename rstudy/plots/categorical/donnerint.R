plot(donner$AGE, donner$NFATE)

pred<-expand.grid(AGE= 1:65, GENDER = levels(donner$GENDER))
pred$fit<-predict(don.mod2, newdata = pred,type = "response")
pred$se<-predict(don.mod2, newdata = pred,type = "response",se = T)$se.fit
dpal = brewer.pal(2,"Set1")

lines(pred[pred$GENDER == "F",]$AGE,pred[pred$GENDER == "F",]$fit,col = dpal[1],lwd = 2)
lines(pred[pred$GENDER == "M",]$AGE,pred[pred$GENDER == "M",]$fit,col = dpal[2],lwd = 2)

abline(v = f.age, col = dpal[1],lty = 3)
abline(v = m.age, col = dpal[2],lty = 3)

legend("topright",legend = c("Female","Male"),col = dpal,lwd = 2)