load("~/Documents/Classes/FAAV_Data/all_philly.RData")
colnames(all_philly)
##  [1] "F1"         "F2"         "F3"         "VCoding"    "Dur_Stress" "Info"       "Name"      
##  [8] "Age"        "Sex"        "Ethnicity"  "Education"  "Street"     "Year"       "Word"      
## [15] "Stress"     "Dur_msec"   "VClass"     "Manner"     "Place"      "Voice"      "PreSeg"    
## [22] "FolSeq"     "File"       "DOB"        "F1.n"       "F2.n"      
setwd("~/Documents/Classes/jofrhwld.github.com/talks/PennState2011/talk/")


eys <- subset(all_philly, VClass %in% c("ey","eyF"))

## I have coded some ey words previously, but we've added more speakers since then.
eys.cod <- read.delim("data/eys.coding.txt")
ey.words <- as.character(unique(eys$Word))
non.coded <- ey.words[!ey.words %in% eys.cod$Word]
#write.table(cbind(non.coded), "data/tocode.txt", sep = "\t", row.names = F, quote = F)
eys.cod.add <- read.delim("data/tocode.txt")

eys.cod <- rbind(eys.cod[,c("Word","Coda","Base","Morph")], eys.cod.add)

## Sanity checks before merge
nrow(eys)
# 42016

sum(!eys$Word %in% eys.cod$Word)
# 0

nrow(merge(eys, eys.cod))
# 42026

eys <- merge(eys, eys.cod)

## Post merge sanity check
nrow(eys)
# 42026

## Eliminate words preceded by *, indicating annotators were unsure of annotation
length(grep("\\*", eys$Word))
# 16

eys <- eys[-grep("\\*", eys$Word),]

## Eliminate words which are NA or ? for Coda coding
with(eys, sum(is.na(Coda)|Coda == "?"))
# 115

eys <- subset(eys, !(is.na(Coda) | Coda == "?"))
eys <- subset(eys, !(is.na(Base) | Base == "?"))

## cleaned data
nrow(eys)
# 41911


## Cleaning coding typos
levels(eys$Base)
# [1] "?"      "closed" "free"   "hiatus" "open"   "final"  "open " 

levels(eys$Base) <- gsub(" ", "", levels(eys$Base))
levels(eys$Base) <- gsub("free", "final", levels(eys$Base))

levels(eys$Coda)
# [1] "?"      "closed" "free"   "hiatus" "open"   "final"  "open "

levels(eys$Coda) <- gsub(" ", "", levels(eys$Coda))


### Eliminate extreaneous levels

as.factor(as.character(eys$Base)) -> eys$Base
as.factor(as.character(eys$Coda)) -> eys$Coda


### Create Diag measure
eys <- transform(eys, Diag = F2.n - F1.n, Diag2 = F2.n - (2*F1.n))

### Evaluate Diag
eys$VClass <- as.character(eys$VClass)
eys.mean <- ddply(eys, .(File, DOB, VClass), summarise, 
                  F1 = mean(F1.n), 
                  F2 = mean(F2.n), 
                  Diag = mean(Diag),
                  Diag2 = mean(Diag2))

compare_diag <- expand.grid(F1 = seq(-1.06, 0.75, length = 100), Diag = seq(-0.21, 2.23, length = 10), Diag2 = seq(-0.96, 3.31, length = 10) )

compare_diag <- transform(compare_diag, F2.1 = Diag + F1, F2.2 = Diag2 + (F1*2))


ggplot(compare_diag, aes(-F2.1, -F1, group = Diag)) + 
        geom_point(data = eys.mean, aes(x = -F2,  color = VClass)) +
        geom_line() + 
        xlim(-1.48, -0.21) + 
        coord_fixed()

ggplot(compare_diag, aes(-F2.2, -F1, group = Diag2)) + 
        geom_point(data = eys.mean, aes(x = -F2,  color = VClass)) +
        geom_line() + 
        xlim(-1.48, -0.21) + 
        coord_fixed()


## Diag is a better fit


## Create DOB variants
eys <- transform(eys, Decade = floor(DOB/10)*10, DOB0 = DOB-min(DOB))
