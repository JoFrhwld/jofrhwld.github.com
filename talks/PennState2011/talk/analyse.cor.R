library(reshape)
####### anae analysis

anae <- load("~/Documents/Classes/Spring_10/misc/anae.Rdata")

anae.c <- cast(mean_anae, TS ~ VClass, value = "ZF2")
anae.cor <- cor(anae.c[, -1], use = "pairwise.complete.obs")

sparsify <- function(mat){
  for(i in 1:nrow(mat)){
    nr <- nrow(mat)
    mat[i,i:nr] <- NA
  }
  return(mat)
}


anae.cor <- sparsify(anae.cor)

anae.cor.df <- melt(anae.cor)
anae.cor.df <- subset(anae.cor.df, !is.na(value))
vowels <- levels(mean_anae$VClass)
anae.cor.df <- transform(anae.cor.df, X1 = vowels[X1], X2 = vowels[X2])
anae.cor.df <- arrange(anae.cor.df, -abs(value))


subset(anae.cor.df, (X1 %in% c("aw","ow","uw") | X2 %in% c("aw","ow","uw")) & value >0)



########## Philly Correlation Analysis

dob.cor <- ddply(all_philly.mean, .(VClass), 
                 summarise, 
                 F1 = cor(F1, DOB, use = "complete.obs"), 
                 F2  = cor(F2, DOB, use = "complete.obs"))


f1.df <- cast(all_philly.mean, File ~ VClass, value = "F1")
f2.df <- cast(all_philly.mean, File ~ VClass, value = "F2")

f1.cor <- cor(f1.df[, -1], use = "pairwise.complete.obs")
f2.cor <- cor(f2.df[, -1], use = "pairwise.complete.obs")


f1.cor <- sparsify(f1.cor)
f2.cor <- sparsify(f2.cor)

f1.cor.df <- melt(f1.cor)
f2.cor.df <- melt(f2.cor)

f1.cor.df <- subset(f1.cor.df, !is.na(value))
f2.cor.df <- subset(f2.cor.df, !is.na(value))

vowels <- levels(all_philly$VClass)

f1.cor.df <- transform(f1.cor.df, X1 = vowels[X1], X2 = vowels[X2])
f2.cor.df <- transform(f2.cor.df, X1 = vowels[X1], X2 = vowels[X2])

f1.cor.df <- arrange(f1.cor.df, -abs(value))
f2.cor.df <- arrange(f2.cor.df, -abs(value))

allophones <- list(
                  c("Tuw","uw", "uwr"), 
                  c("ay","ay0"), 
                  c("ey","eyF", "eyr"), 
                  c("iy","iyF", "iyr"),
                  c("ow","owF", "owr"))

for(set in  allophones){
  f1.cor.df <- subset(f1.cor.df, !(X1 %in% set & X2 %in% set))
  f2.cor.df <- subset(f2.cor.df, !(X1 %in% set & X2 %in% set))
}



wilcox.test(f1.cor.df$value, conf.int = T)
#   Wilcoxon signed rank test with continuity correction
# 
# data:  f1.cor.df$value 
# V = 52712, p-value = 1.444e-06
# alternative hypothesis: true location is not equal to 0 
# 95 percent confidence interval:
#  0.02859541 0.06757959 
# sample estimates:
# (pseudo)median 
#     0.04788585 


wilcox.test(f2.cor.df$value, conf.int = T)
#   Wilcoxon signed rank test with continuity correction
# 
# data:  f2.cor.df$value 
# V = 47359, p-value = 0.01058
# alternative hypothesis: true location is not equal to 0 
# 95 percent confidence interval:
#  0.006413416 0.049058503 
# sample estimates:
# (pseudo)median 
#     0.02734928 
                  
                  
h_b_coding <- data.frame(
  VClass = c("*hr", "Tuw", "ae", "aeBR", "aeh", "ah", "ahr", "aw", "ay", 
"ay0", "e", "ey", "eyF", "eyr", "i", "iw", "iy", "iyF", "iyr", 
"o", "oh", "ow", "owF", "owr", "oy", "u", "uh", "uw", "uwr"), 
  Height = c(NA, 1, 3, NA, 2, 3, 2, 2, 3, 
NA, 2, 2, 2, 2, 1, 1, 1, 1, 1, 
3, 2, 2, 2, 1, NA, 1, 2, 1, 1),                  
  Backness = c(NA, 1, 1, NA, 1, 2, 2, 1, 2, 
2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
2, 2, 1, 1, 2, 2, 2, 2, 1, 2))

head(subset(f1.cor.df, value > 0), 10)
#     X1  X2     value  Height    Back
# 2   oh aeh 0.6562171  +         -
# 3    e *hr 0.6470978  +         ?
# 4  iyF  iw 0.6331352  +         +
# 5    o  ay 0.5974071  +         +
# 7    o  ae 0.5338104  +         -
# 8  iyF Tuw 0.5230680  +         change
# 9  owr ahr 0.5133348  +         +
# 10  ey ay0 0.5067550  change    -
# 11  ay  ae 0.5029559  +         -
# 14  iy Tuw 0.4779909  +         change


f1.cor.df <- merge(f1.cor.df, h_b_coding, by.x = "X1", by.y = "VClass")
f1.cor.df <- merge(f1.cor.df, h_b_coding, by.x = "X2", by.y = "VClass")             
f1.cor.df <- transform(f1.cor.df, Height = Height.x == Height.y, Backness = Backness.x == Backness.y) 
f1.cor.m <- melt(f1.cor.df,  id = "value", measure = c("Height","Backness"))
colnames(f1.cor.m) <- c("F1.cor", "Feature", "value")
             
                  
head(subset(f2.cor.df, value > 0), 10)
#     X1  X2     value    Height      Back
# 1   oh ahr 0.7532825    +           +
# 3  owr  oh 0.6973869    +           +
# 4   ay ahr 0.6176820    1           +
# 7    o  ay 0.5580746    +           +
# 10 owF  aw 0.5269725    change      change
# 11   e  ae 0.5159874    1           +
# 12  iy  ey 0.5155642    1           +
# 14  oy  oh 0.5135550    1           +
# 15 ay0 ahr 0.5128929    change      +
# 17  oy ahr 0.4882139    1           +

             
f2.cor.df <- merge(f2.cor.df, h_b_coding, by.x = "X1", by.y = "VClass")
f2.cor.df <- merge(f2.cor.df, h_b_coding, by.x = "X2", by.y = "VClass")             
f2.cor.df <- transform(f2.cor.df, Height = Height.x == Height.y, Backness = Backness.x == Backness.y) 
f2.cor.m <- melt(f2.cor.df,  id = "value", measure = c("Height","Backness"))
colnames(f2.cor.m) <- c("F2.cor", "Feature", "value") 
            
head(subset(f1.cor.df, value < 0), 10)
#     X1  X2      value    Height     Back
# 6    o Tuw -0.5468753    2          change
# 12 owF iyF -0.4940287    1          change
# 20 iyF  ae -0.4260342    2          +
# 23   o  ey -0.4153908    1          -
# 25 owF  iy -0.4104439    1          change
# 26  ay *hr -0.4094534    ?          ?
# 38  ow  iy -0.3768554    1          change
# 43 owF Tuw -0.3563480    1          change
# 44 iyr   e -0.3534210    1          +
# 45 owF iyr -0.3500530    1          change

head(subset(f2.cor.df, value < 0), 10)
#     X1  X2      value    Height   Back
# 5  owF ahr -0.5952890    ?        change
# 6  owF ay0 -0.5661111    ?        change
# 8   aw ahr -0.5379820    ?        -
# 13  ay aeh -0.5148472    1        -
# 19  oh  aw -0.4813532    ?        -        
# 21 owF  ay -0.4547997    1        change
# 22  oh  iy -0.4542608    1        -
# 29  oh  ey -0.4328341    +        -
# 30 owF  oh -0.4328272    +        change
# 32 iyF ay0 -0.4182473    2-       -

f2.pos.cor <- f2.cor.df
f2.pos.cor <- merge(f2.pos.cor, h_b_coding, by.x = "X1", by.y = "VClass")
f2.pos.cor <- merge(f2.pos.cor, h_b_coding, by.x = "X2", by.y = "VClass")             
f2.pos.cor <- transform(f2.pos.cor, HMatch = Height.x == Height.y, BMatch = Backness.x == Backness.y) 

