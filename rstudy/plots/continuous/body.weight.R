library(ISwR)
attach(rmr)

plot(density(body.weight))
stripchart(body.weight, add = T, at = 0)
abline(v = mean(body.weight),lty = 2)
abline(v = median(body.weight),lty = 3)
legend("topright",legend = c("mean","median"),lty = c(2,3))