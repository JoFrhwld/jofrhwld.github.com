columns <- c("Word", "seg","SegTrans", "DepVar","td",
             "POS","Gram","Gram2", "PreSegTrans", "FolSegTrans", 
             "PreSeg", "FolSeg", "Speaker", "Recording", "DictNSyl", 
             "NSyl", "DictRate", "Rate", "FolWord",  "freq", 
             "log_freq","Context")

buck <- buck[, columns]


split_write <- function(df, fn.col="Speaker", dir = "talks/PennState2011/r/data/buckeye/",...){
  fname <- paste(as.character(df[1,fn.col]), "txt", sep = ".")
  write.table(df, file.path(dir,fname), ...)
}

library(plyr)
d_ply(buck, .(Speaker), split_write, .progress = "text", row.names = F, quote = F, sep = "\t")

