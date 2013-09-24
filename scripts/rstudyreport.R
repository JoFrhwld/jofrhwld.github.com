#######
### Load the necessary Libraries and Scripts
#######


## install.packages(c("RColorBrewer","gdata"))
library(RColorBrewer)
library(gdata)
source("http://www.ling.upenn.edu/~joseff/scripts/recontrast.R")

######
## Define a Color Palate
######
pal <- rev(brewer.pal(3,"Paired")[1:2])


############################################################
##Begin ING


######
## Read in the ING Data
######
ing <- read.csv("http://www.ling.upenn.edu/~joseff/rstudy/data/ing2.csv")

####
## GramStatus Proportion Table
###
ing.gram.table <- table(ing$DepVar, ing$GramStatus)
prop.table(ing.gram.table,2)


####
## Plot the GramStatus Proportion Table
####
barplot(prop.table(ing.gram.table,2), las = 2,space = 0,col = pal,axisnames = F)
ngram <- table(ing$GramStatus)
text(1:nlevels(ing$GramStatus)-0.5, rep(0.9, nlevels(ing$GramStatus)), paste("n = \n",ngram))
text(1:nlevels(ing$GramStatus)-0.5, rep(0, nlevels(ing$GramStatus)), colnames(ing.gram.table), adj = c(0,0.3), srt = 90)
abline(h = 0.5, col = "red")



####
## GramStatus pairwise test
####

pair.gram1 <- pairwise.prop.test(t(ing.gram.table), p.adj = "bonf")
pair.gram1



####
## Collapse GramStatus Categories
####

ing$Gram2 <- as.character(ing$GramStatus)
ing[ing$GramStatus %in% c("participle","progressive"),]$Gram2 <- "verbal"
ing[ing$GramStatus %in% c("adjective","noun"),]$Gram2 <- "noun"
ing$Gram2 <- as.factor(ing$Gram2)



####
## Check new GramStatus Proportions
###

ing.gram.table2 <- table(ing$DepVar, ing$Gram2)
prop.table(ing.gram.table2,2)


####
## Plot New GramStatus Proportions
####

barplot(prop.table(ing.gram.table2,2), las = 2,space = 0,col = pal,axisnames = F)
ngram2 <- table(ing$Gram2)
text(1:nlevels(ing$Gram2)-0.5, rep(0.9, nlevels(ing$Gram2)), paste("n = \n",ngram2))
text(1:nlevels(ing$GramStatus)-0.5, rep(0, nlevels(ing$GramStatus)), colnames(ing.gram.table2), adj = c(0,0.3), srt = 90)
abline(h = 0.5, col = "red")

####
## New GramStatus Pairwise Prop Test
####
pair.gram2 <- pairwise.prop.test(t(ing.gram.table2), p.adj = "bonf")
pair.gram2



####
## FolSeg Proportions
####

ing.fol.table1 <- table(ing$DepVar, ing$Following.Seg)
prop.table(ing.fol.table1,2)

####
## Plot FolSeg Proportions
####

barplot(prop.table(ing.fol.table1,2), las = 2,space = 0,col = pal,axisnames = F)
nfol <- table(ing$Following.Seg)
text(1:nlevels(ing$Following.Seg)-0.5, rep(0.9, nlevels(ing$Following.Seg)), paste("n = \n",nfol))
text(1:nlevels(ing$Following.Seg)-0.5, rep(0, nlevels(ing$GramStatus)), colnames(ing.fol.table1), adj = c(0,0.3), srt = 90)
abline(h = 0.5, col = "red")

####
## FolSeg Pairwise PropTest
####

pair.fol1 <- pairwise.prop.test(t(ing.fol.table1), p.adj = "bonf")
pair.fol1

####
## Collapse FolSeg Categories
####

ing$Tongue <- "other"
ing[ing$Following.Seg %in% c("palatal","velar"),]$Tongue <- "posterior"
ing[ing$Following.Seg == "apical",]$Tongue <- "apical"
ing$Tongue <- as.factor(ing$Tongue)
ing$Tongue <- relevel(ing$Tongue, "other")

####
## New FolSeg Proportions
####
ing.fol.table2 <- table(ing$DepVar, ing$Tongue)
prop.table(ing.fol.table2,2)

####
## Plot New FolSeg Proportions
####
barplot(prop.table(ing.fol.table2,2), las = 2,space = 0,col = pal,axisnames = F)
nfol <- table(ing$Tongue)
text(1:nlevels(ing$Tongue)-0.5, rep(0.9, nlevels(ing$Tongue)), paste("n = \n",nfol))
text(1:nlevels(ing$Tongue)-0.5, rep(0, nlevels(ing$Tongue)), colnames(ing.fol.table2), adj = c(0,0.3), srt = 90)

abline(h = 0.5, col = "red")

####
## New FolSeg Pairwise PropTest
####

pair.fol2 <- pairwise.prop.test(t(ing.fol.table2), p.adj = "bonf")

####
## Style Proportions
####
ing.sty.table <- table(ing$DepVar, ing$Style)
prop.table(ing.sty.table,2)

####
## Plot Style Proportions
####
par(mar = c(6,4,4,2)+0.1)
barplot(prop.table(ing.sty.table,2), las = 2, space = 0,col = pal, axisnames = F)
nstyle <- table(ing$Style)
text(1:nlevels(ing$Style)-0.5, rep(0.9, nlevels(ing$Style)), paste("n = \n",nstyle))
text(1:nlevels(ing$Style)-0.5, rep(0, nlevels(ing$Style)), colnames(ing.sty.table), adj = c(0,0.3), srt = 90)
abline(h = 0.5, col = "red")

####
## Collapse Style Categories
####
ing$Style2 <- as.character(ing$Style)
ing[ing$Style %in% c("careful","response"),]$Style2 <- "careful"
ing[ing$Style %in% c("narrative","tangent"),]$Style2 <- "narrative"
ing[ing$Style %in% c("language","kids","group"),]$Style2 <- "other"
ing$Style2 <- as.factor(ing$Style2)

####
## New Style Proportions
####
ing.sty.table2 <- table(ing$DepVar, ing$Style2)
prop.table(ing.sty.table2,2)

####
## Plot New Style Categories
####
barplot(prop.table(ing.sty.table2,2), las = 2, space = 0,col = pal, axisnames = F)
nstyle <- table(ing$Style2)
text(1:nlevels(ing$Style2)-0.5, rep(0.9, nlevels(ing$Style2)), paste("n = \n",nstyle))
text(1:nlevels(ing$Style2)-0.5, rep(0, nlevels(ing$Style2)), colnames(ing.sty.table2), adj = c(0,0.3), srt = 90)
abline(h = 0.5, col = "red")


####
## New Style Pairwise Prop Tes
####

pair.sty <- pairwise.prop.test(t(ing.sty.table2), p.adj = "bonf")

####
## Fit Models for Collapse Comparisons
####

model1 <- glm(DepVar~GramStatus + Following.Seg + Style, data = ing, family = binomial)
model2 <- glm(DepVar~Gram2 + Following.Seg + Style, data = ing, family = binomial)
model3 <- glm(DepVar~Gram2 + Tongue + Style, data = ing, family = binomial)
model4 <- glm(DepVar~Gram2 + Tongue + Style2, data = ing, family = binomial)

####
## Stepwise Collapse Testing (analysis of Deviance)
####

collapseTest <- anova(model1, model2, model3, model4, test = "Chisq")
collapseTest

####
## Catastrophic Collapse Testing (analysis of Deviance)
####

totalCollapseTest <- anova(model1, model4, test = "Chisq")
totalCollapseTest

####
## Factor Group Significance
####

factorGroups <- anova(model4, test = "Chisq")
factorGroups

####
## Manipulate Levels and set Proper Contrasts
####
#Treatment
levels(ing$Tongue)
ing$Tongue <- recontrast(ing$Tongue, "treatment")
#Sum
ing$Gram2 <- recontrast(ing$Gram2, "sum")
ing$Style2 <- recontrast(ing$Style2, "sum")

####
## First Model
####

model4 <- glm(DepVar~Gram2 + Tongue + Style2, data = ing, family = binomial)
summary(model4)

####
## Set Up the Coefficient Dot Plot
####

coefs <- model4$coef
int <- coefs[1]
gram <- coefs[2:5]
tongue <- coefs[6:7]
style <- coefs[8:10]
gram <- c(gram, verbal = sum(gram)*-1)
tongue <- c(other = 0 ,tongue)
style <- c(style, soapbox = sum(style)*-1)

names(gram) <- paste("Gram: ",levels(ing$Gram2))
names(tongue) <- paste("Tongue:", levels(ing$Tongue))
names(style) <- paste("Style:", levels(ing$Style2))

gram <- sort(gram)
tongue <- sort(tongue)
style <- sort(style)


ncoefs = c(int,gram,tongue,style)
groups = c(1,rep(2,length(gram)),rep(3,length(tongue)), rep(4,length(style)))

####
## Plot Coefficients DotPlot
####
par(mar = c(5,4,2,2)+0.1)
dotchart(ncoefs, groups = groups, pch = 19)
abline(v = 0)

####
## Recontrast and Refit
####

ing$Style2 <- relevel(ing$Style2, "careful")
ing$Style2 <- recontrast(ing$Style2, "treatment")
ing$Tongue <- recontrast(ing$Tongue, "treatment")
ing$Gram2 <- recontrast(ing$Gram2, "Sum")

model4.2 <- glm(DepVar~Gram2 + Tongue + Style2, data = ing, family = binomial)
summary(model4.2)

####
## Set Up  new DotPlot
####
coefs <- model4.2$coef
int <- coefs[1]
gram <- coefs[2:5]
tongue <- coefs[6:7]
style <- coefs[8:10]
gram <- c(gram, verbal = sum(gram)*-1)
tongue <- c(other = 0 ,tongue)
style <- c(0, style)

names(gram) <- paste("Gram: ",levels(ing$Gram2))
names(tongue) <- paste("Tongue:", levels(ing$Tongue))
names(style) <- paste("Style:", levels(ing$Style2))

gram <- sort(gram)
tongue <- sort(tongue)
style <- sort(style)


ncoefs = c(int,gram,tongue,style)
groups = c(1,rep(2,length(gram)),rep(3,length(tongue)), rep(4,length(style)))

####
## Plot New DotPlot
####
par(mar = c(5,4,2,2)+0.1)
dotchart(ncoefs, groups = groups, pch = 19)
abline(v = 0)



################################
## Interactions Illustration

fake <- expand.grid(Category = c("A","B"), Dimension = 1:10)
fake$Resp = fake$Dimension * 2

plot(fake$Dimension, fake$Resp, type = "n")
for(i in 1:nlevels(fake$Category)){
	.sub <- subset(fake, Category == levels(fake$Category)[i])
	lines(.sub$Dimension, .sub$Resp, col = i)
}
legend("topleft", legend = c("Cat = n.s.", "Cat:Dim = n.s."))

fake <- expand.grid(Category = c("A","B"), Dimension = 1:10)
fake$Resp = fake$Dimension * 2
fake[fake$Category == "A",]$Resp  =  fake[fake$Category == "A",]$Resp + 3

plot(fake$Dimension, fake$Resp, type = "n")
for(i in 1:nlevels(fake$Category)){
	.sub <- subset(fake, Category == levels(fake$Category)[i])
	lines(.sub$Dimension, .sub$Resp, col = i)
}
legend("topleft", legend = c("Cat < 0.05", "Cat:Dim = n.s."))

fake <- expand.grid(Category = c("A","B"), Dimension = 1:10)
fake$Resp = fake$Dimension * 2
fake[fake$Category == "A",]$Resp  =  fake[fake$Category == "A",]$Resp + 3
fake[fake$Category == "B",]$Resp  =  fake[fake$Category == "B",]$Resp*0.7

plot(fake$Dimension, fake$Resp, type = "n")
for(i in 1:nlevels(fake$Category)){
	.sub <- subset(fake, Category == levels(fake$Category)[i])
	lines(.sub$Dimension, .sub$Resp, col = i)
}
legend("topleft", legend = c("Cat < 0.05", "Cat:Dim  < 0.05"))


fake <- expand.grid(Category = c("A","B"), Dimension = 1:10)
fake$Resp = fake$Dimension * 2
fake[fake$Category == "A",]$Resp  =  fake[fake$Category == "A",]$Resp - 0.6
fake[fake$Category == "B",]$Resp  =  fake[fake$Category == "B",]$Resp*0.7



plot(fake$Dimension, fake$Resp, type = "n")
for(i in 1:nlevels(fake$Category)){
	.sub <- subset(fake, Category == levels(fake$Category)[i])
	lines(.sub$Dimension, .sub$Resp, col = i)
}
legend("topleft", legend = c("Cat < n.s.", "Cat:Dim  < 0.05"))

##End Interactions Illustration
###############################################



####
## Morphology~Phonology Interactions?
####

model5 <- glm(DepVar ~ Gram2 + Tongue + Style2 + Gram2:Tongue, data = ing, family = binomial)
anova(model5, test = "Chisq")

####
## Style~Morphology Interaction?
####

model6 <- glm(DepVar ~ Gram2 + Tongue + Style2 + Style2:Gram2, data = ing, family = binomial)
anova(model6, test = "Chisq")

####
## Style~Phonology Interactions?
####

model7 <- glm(DepVar ~ Gram2 + Tongue + Style2 + Style2:Tongue, data = ing, family = binomial)
anova(model7, test = "Chisq")

##End ING
############################################################

############################################################
## Begin TD


####
## Load the TD data
####
td <- read.csv("http://www.ling.upenn.edu/~joseff/rstudy/data/sbuck.csv")

####
## Reshape the data appropriately
####
td$Gram <- relevel(td$Gram,"Past")
td$Gram <- recontrast(td$Gram)

####
## Lexical Exclusions
####
excep <-  td[(td$Word %in% c("don't","just","kind")),]
excep <- drop.levels(excep)
td <- td[!(td$Word %in% c("don't","just","kind")),]

####
## Slope Model ( ~ Log_Freq) 
####

model1 <- glm(DepVar ~ Log_Freq, data = td, family = binomial)
anova(model1, test = "Chisq")


#####
## Get fitted values for plotting
####
grid <- expand.grid(Log_Freq = seq(min(td$Log_Freq),max(td$Log_Freq), length = 100))
pal <- brewer.pal(4,"Set1")

grid$Resp <- predict(model1, newdata = grid, type = "response", se = T)$fit
grid$SE <- predict(model1, newdata = grid, type = "response", se = T)$se.fit

####
## Plot Fitted Values
####

plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1))
lines(grid$Log_Freq, grid$Resp)
lines(grid$Log_Freq, grid$Resp+(1.96*grid$SE), lty= 2)
lines(grid$Log_Freq, grid$Resp-(1.96*grid$SE), lty= 2)



####
## Add Gram Intercepts ( ~ Gram + Log_Freq)
####

model2 <- glm(DepVar ~ Gram + Log_Freq, data = td, family = binomial)
anova(model2, test = "Chisq")

####
## Get Fitted Values for Plotting
## This block is so long because I'm clipping the plotted
## lines to the frequency range observed for each
## grammatical class
####

minp <- min(td[td$Gram == "Past",]$Log_Freq)
maxp <- max(td[td$Gram == "Past",]$Log_Freq)

minm <- min(td[td$Gram == "Mono",]$Log_Freq)
maxm <- max(td[td$Gram == "Mono",]$Log_Freq)

mind <- min(td[td$Gram == "Irreg",]$Log_Freq)
maxd <- max(td[td$Gram == "Irreg",]$Log_Freq)

minn <- min(td[td$Gram == "n't",]$Log_Freq)
maxn <- max(td[td$Gram == "n't",]$Log_Freq)


grid <- expand.grid(Gram = levels(td$Gram), Log_Freq = seq(min(td$Log_Freq),max(td$Log_Freq), length = 100))

grid$Resp <- predict(model2, newdata = grid, type = "response", se = T)$fit
grid$SE <- predict(model2, newdata = grid, type = "response", se = T)$se.fit

gridp <- grid[grid$Gram == "Past" & grid$Log_Freq >= minp & grid$Log_Freq <= maxp,]
gridd <- grid[grid$Gram == "Irreg" & grid$Log_Freq >= mind & grid$Log_Freq <= maxd,]
gridm <- grid[grid$Gram == "Mono" & grid$Log_Freq >= minm & grid$Log_Freq <= maxm,]
gridn <- grid[grid$Gram == "n't" & grid$Log_Freq >= minn & grid$Log_Freq <= maxn,]

grid <- rbind(gridp,gridd,gridm,gridn)

####
## Plot ~Gram+Log_Freq Fitted Values
####
plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1))

for(i in 1:nlevels(td$Gram)){
	sub <- subset(grid, Gram == levels(td$Gram)[i])
	
	lines(sub$Log_Freq, sub$Resp, lwd = 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp+(1.96*sub$SE), lty= 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp-(1.96*sub$SE), lty= 2, col = pal[i])
}
legend("topright",legend = levels(td$Gram), col = pal, lwd = 2)


####
## Add Slopes for each Gram ( ~ Gram + Log_Freq + Gram:Log_Freq)
####
model3 <- glm(DepVar ~ Gram + Log_Freq + Gram:Log_Freq, data = td, family = binomial)
anova(model3, test = "Chisq")

####
## Get unique slopes fitted values
####
grid <- expand.grid(Gram = levels(td$Gram), Log_Freq = seq(min(td$Log_Freq),max(td$Log_Freq), length = 100))

grid$Resp <- predict(model3, newdata = grid, type = "response", se = T)$fit
grid$SE <- predict(model3, newdata = grid, type = "response", se = T)$se.fit

gridp <- grid[grid$Gram == "Past" & grid$Log_Freq >= minp & grid$Log_Freq <= maxp,]
gridd <- grid[grid$Gram == "Irreg" & grid$Log_Freq >= mind & grid$Log_Freq <= maxd,]
gridm <- grid[grid$Gram == "Mono" & grid$Log_Freq >= minm & grid$Log_Freq <= maxm,]
gridn <- grid[grid$Gram == "n't" & grid$Log_Freq >= minn & grid$Log_Freq <= maxn,]

grid <- rbind(gridp,gridd,gridm,gridn)

####
## Plot Unique Slopes
####

plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1))

for(i in 1:nlevels(td$Gram)){
	sub <- subset(grid, Gram == levels(td$Gram)[i])
	
	lines(sub$Log_Freq, sub$Resp, lwd = 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp+(1.96*sub$SE), lty= 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp-(1.96*sub$SE), lty= 2, col = pal[i])
}
legend("topright",legend = levels(td$Gram), col = pal, lwd = 2)

####
## Interaction Coefficients
####

summary(model3)


####
## Are Mono, Irreg and n't same?
####

####
## Take Subset
####
subTd <- subset(td, Gram != "Past")
subTd <- drop.levels(subTd)
subTd$Gram <- relevel(subTd$Gram,"Mono")
subTd$Gram <- recontrast(subTd$Gram, "treatment")

####
## Subset Slopes
####

model4 <- glm(DepVar ~ Gram + Log_Freq + Gram:Log_Freq, data = subTd, family = binomial)
anova(model4, test = "Chisq")

####
## Recode for Past Tense vs Other
####
td$Gram2 <- "other"
td[td$Gram == "Past",]$Gram2 <- "past"
td$Gram2 <- as.factor(td$Gram2)

####
##  Relevel and Recontrast
####
td$Gram <- relevel(td$Gram, "Mono")
td$Gram <- recontrast(td$Gram, "treatment")

####
## Fit the refined model
####

model5 <- glm(DepVar ~ Gram2 + Gram2:Gram + Gram2:Log_Freq -1 , data = td, family = binomial)
summary(model5)

####
## Get fitted values for refined model
####

grid <- expand.grid(Gram = levels(td$Gram), Log_Freq = unique(td$Log_Freq))


gridp <- grid[grid$Gram == "Past" & grid$Log_Freq >= minp & grid$Log_Freq <= maxp,]
gridp$Gram2 <- "past"
gridd <- grid[grid$Gram == "Irreg" & grid$Log_Freq >= mind & grid$Log_Freq <= maxd,]
gridd$Gram2 <- "other"
gridm <- grid[grid$Gram == "Mono" & grid$Log_Freq >= minm & grid$Log_Freq <= maxm,]
gridm$Gram2 <- "other"
gridn <- grid[grid$Gram == "n't" & grid$Log_Freq >= minn & grid$Log_Freq <= maxn,]
gridn$Gram2 <- "other"

grid <- rbind(gridp,gridd,gridm,gridn)
grid$Gram2 <- as.factor(grid$Gram2)

grid$Resp <- predict(model5, newdata = grid, type = "response", se = T)$fit
grid$SE <- predict(model5, newdata = grid, type = "response", se = T)$se.fit
grid <- grid[order(grid$Log_Freq),]
grid$Gram <- relevel(grid$Gram, "Irreg")
grid$Gram <- relevel(grid$Gram, "Past")

####
## Plot Refined Model
####
plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1))

for(i in 1:nlevels(grid$Gram)){
	sub <- subset(grid, Gram == levels(grid$Gram)[i])
	
	lines(sub$Log_Freq, sub$Resp, lwd = 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp+(1.96*sub$SE), lty= 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp-(1.96*sub$SE), lty= 2, col = pal[i])
}
legend("topright",legend = levels(grid$Gram), col = pal, lwd = 2)




## End TD
############################################################


############################
## Appendix
plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1), xlim = range(td$Log_Freq, excep$Log_Freq))

for(i in 1:nlevels(grid$Gram)){
	sub <- subset(grid, Gram == levels(grid$Gram)[i])
	
	lines(sub$Log_Freq, sub$Resp, lwd = 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp+(1.96*sub$SE), lty= 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp-(1.96*sub$SE), lty= 2, col = pal[i])
}
legend("topright",legend = levels(grid$Gram), col = pal, lwd = 2)

ef <- tapply(excep$Log_Freq, excep$Word, mean)
ed <- tapply(excep$DepVar, excep$Word, mean)

points(ef, ed, pch = 19, col = pal[c(4,2,2)])

##############
summary(model4)

#############

td$Gram3 <- "im"
td[td$Gram == "Past",]$Gram3 <- "past"
td[td$Gram == "n't",]$Gram3 <- "n't"
td$Gram2 <- as.factor(td$Gram3)

###############

model6 <- glm(DepVar ~ Gram3 + Gram3:Gram + Gram3:Log_Freq -1 , data = td, family = binomial)

###############

grid <- expand.grid(Gram = levels(td$Gram), Log_Freq = unique(td$Log_Freq))


gridp <- grid[grid$Gram == "Past" & grid$Log_Freq >= minp & grid$Log_Freq <= maxp,]
gridp$Gram3 <- "past"
gridd <- grid[grid$Gram == "Irreg" & grid$Log_Freq >= mind & grid$Log_Freq <= maxd,]
gridd$Gram3 <- "im"
gridm <- grid[grid$Gram == "Mono" & grid$Log_Freq >= minm & grid$Log_Freq <= maxm,]
gridm$Gram3 <- "im"
gridn <- grid[grid$Gram == "n't" & grid$Log_Freq >= minn & grid$Log_Freq <= maxn,]
gridn$Gram3 <- "n't"

grid <- rbind(gridp,gridd,gridm,gridn)
grid$Gram3 <- as.factor(grid$Gram2)

grid$Resp <- predict(model6, newdata = grid, type = "response", se = T)$fit
grid$SE <- predict(model6, newdata = grid, type = "response", se = T)$se.fit
grid <- grid[order(grid$Log_Freq),]
grid$Gram <- relevel(grid$Gram, "Irreg")
grid$Gram <- relevel(grid$Gram, "Past")


plot(grid$Log_Freq, grid$Resp, type = "n", xlab = "Log(Freq)", ylab = "p(td)",ylim = c(0,1), xlim = range(td$Log_Freq, excep$Log_Freq))

for(i in 1:nlevels(grid$Gram)){
	sub <- subset(grid, Gram == levels(grid$Gram)[i])
	
	lines(sub$Log_Freq, sub$Resp, lwd = 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp+(1.96*sub$SE), lty= 2, col = pal[i])
	lines(sub$Log_Freq, sub$Resp-(1.96*sub$SE), lty= 2, col = pal[i])
}
legend("topright",legend = levels(grid$Gram), col = pal, lwd = 2)

ef <- tapply(excep$Log_Freq, excep$Word, mean)
ed <- tapply(excep$DepVar, excep$Word, mean)

points(ef, ed, pch = 19, col = pal[c(4,2,2)])


