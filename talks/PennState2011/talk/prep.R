load("~/Documents/Classes/FAAV_Data/all_philly.RData")
colnames(all_philly)
##  [1] "F1"         "F2"         "F3"         "VCoding"    "Dur_Stress" "Info"       "Name"      
##  [8] "Age"        "Sex"        "Ethnicity"  "Education"  "Street"     "Year"       "Word"      
## [15] "Stress"     "Dur_msec"   "VClass"     "Manner"     "Place"      "Voice"      "PreSeg"    
## [22] "FolSeq"     "File"       "DOB"        "F1.n"       "F2.n"      
setwd("~/Documents/Classes/jofrhwld.github.com/talks/PennState2011/talk/")


eys <- subset(all_philly, VClass %in% c("ey","eyF"))

## I have coded some ey words previously, but we've added more speakers since then
eys.cod <- read.delim("data/eys.coding.txt")
ey.words <- as.character(unique(eys$Word))
non.coded <- ey.words[!ey.words %in% eys.cod$Word]
write.table(cbind(non.coded), "data/tocode.txt", sep = "\t", row.names = F, quote = F)



