##################################################################################
# This is script is meant to plot in an illuminating way the data as published with 
# the Atlas of North American English (Labov, Ash and Boberg 2006), specifically
# with the data file "telsur_439.xls." To use this script, you will need access to
# this data file.
#
# Usage:
# anae.plot(telsur, output)
# telsur = The full path to the "telsur_439.xls"
# output = An already existing output directory.  The script automatically organizes
# 		   the output into directories and subdirectories, so this will not clutter the 
#		   the designated directory.
#
# Required Packages:gdata
#
# If there is an error involving making directories at the /usr/ level, this 
# probably means gdata is not installed, and you do not have administrative 
# permissions to install it yourself.
#
# Copyright Josef Fruehwald 2009
#	This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#	 along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
##################################################################################




anae.plot <- function(input, output){
	get.packages()	
	anae <<- read.xls(input)
	reformat(anae)->>anae
	spcom.format(anae)->>anae
	anae[anae$F1 > 0,]->>anae
	a.plot(anae, output)
}

a.plot<-function(data, directory){

	
x11()
dev.off()
paste(directory, "ANAE", sep = "/")->location
system(paste("mkdir", location, sep = " "))
setwd(location)
for(i in levels(data$DIALECT)){
	reg = data[data$DIALECT == i,]
	drop.levels(reg)->reg
	paste(location, i, sep = "/")->jawn1
	paste(jawn1, "SPCOM", sep = "/")->jawn2
	
	system(paste("mkdir", jawn1, sep = " "))
	system(paste("mkdir", jawn2, sep = " "))



	for(j in levels(reg$SPCOM)){
		paste(j,"pdf", sep = ".") ->name
		paste(i,"SPCOM", name, sep = "/")->fi
		sp = reg[reg$SPCOM == j,]
		drop.levels(sp$TS)->sp$TS
		
		pdf(fi,5,5)
		n = length(levels(as.factor(sp$TS)))
		paste("N =", n, sep = " ")->N
		paste(j, N, sep = " ")->title
		
		
		plot(1,1, xlim = rev(range(anae$F2)), ylim = rev(range(anae$F1)), ylab = "F1", xlab = "F2", main = title)		
		sp = reg[reg$SPCOM == j,]
		sp$VClass = drop.levels(sp$VClass)
		
		spout = matrix(,length(levels(sp$VClass)),3)
		k = 1
		for(l in levels(sp$VClass)){
			mF1 = mean(sp[sp$VClass == l,]$F1)
			mF2 = mean(sp[sp$VClass == l,]$F2)
			
			str = c(mF1, mF2, l)
			
			spout[k,]<- str
			
			k = k+1
		}
		as.data.frame(spout)->spout
		colnames(spout)<- c("F1","F2","VClass")
		as.numeric(as.character(spout$F1))->spout$F1
		as.numeric(as.character(spout$F2))->spout$F2
		
		rbind(spout[spout$VClass == "i", ],
			spout[spout$VClass == "e", ],
			spout[spout$VClass == "ae", ],
			spout[spout$VClass == "o", ],
			spout[spout$VClass == "uh",],
			spout[spout$VClass == "u", ]		
		)->short
		
		rbind(spout[spout$VClass == "iyc", ],
			spout[spout$VClass == "eyc", ],
			spout[spout$VClass == "ayv", ]
			#spout[spout$VClass == "oy",]
		)->front
		
		rbind(spout[spout$VClass == "ayv", ],
			spout[spout$VClass == "ay0", ]
		)->ays
	
		rbind(spout[spout$VClass == "iw", ],
			spout[spout$VClass == "uwf", ],
			spout[spout$VClass == "uwc", ],
			spout[spout$VClass == "owc", ],
			spout[spout$VClass == "aw", ]
		)->back
	
		rbind(spout[spout$VClass == "iyr", ],
			spout[spout$VClass == "eyr", ],
			spout[spout$VClass == "aeh", ]
		)->frontin
	
		rbind(spout[spout$VClass == "uwr", ],
			spout[spout$VClass == "owr", ],
			spout[spout$VClass == "ohr", ],
			spout[spout$VClass == "oh", ],
			spout[spout$VClass == "ahr", ],
			spout[spout$VClass == "ah", ]
		)->backin	
		
		polygon(short$F2, short$F1)
		lines(front$F2,front$F1, col = "red")
		lines(back$F2, back$F1, col = "blue")
		lines(frontin$F2, frontin$F1, lty = 2, col = "red")
		lines(backin$F2, backin$F1, lty = 2, col = "blue")	
		
		rbind(short, front, back, frontin, backin)->pted
		text(pted$F2, pted$F1, pted$VClass, cex = .75)
		
		dev.off()
		print(paste(j, "Plotted and Saved", sep = " "))
	}
	paste(i,"pdf", sep = ".") ->name
	paste(i, name, sep = "/")->fi

	pdf(fi, 5,5)
	plot(1,1, xlim = rev(range(anae$F2)), ylim = rev(range(anae$F1)), ylab = "F1", xlab = "F2", main = i)		
		
		
	spout = matrix(,length(levels(reg$VClass)),3)
	k = 1
	for(l in levels(reg$VClass)){
		mF1 = mean(reg[reg$VClass == l,]$F1)
		mF2 = mean(reg[reg$VClass == l,]$F2)
			
		str = c(mF1, mF2, l)
			
		spout[k,]<- str
			
		k = k+1
	}
	as.data.frame(spout)->spout
	colnames(spout)<- c("F1","F2","VClass")
	as.numeric(as.character(spout$F1))->spout$F1
	as.numeric(as.character(spout$F2))->spout$F2
		
		rbind(spout[spout$VClass == "i", ],
			spout[spout$VClass == "e", ],
			spout[spout$VClass == "ae", ],
			spout[spout$VClass == "o", ],
			spout[spout$VClass == "uh",],
			spout[spout$VClass == "u", ]		
		)->short
		
		rbind(spout[spout$VClass == "iyc", ],
			spout[spout$VClass == "eyc", ],
			spout[spout$VClass == "ayv", ]
			#spout[spout$VClass == "oy",]
		)->front
		
		rbind(spout[spout$VClass == "ayv", ],
			spout[spout$VClass == "ay0", ]
		)->ays
	
		rbind(spout[spout$VClass == "iw", ],
			spout[spout$VClass == "uwf", ],
			spout[spout$VClass == "uwc", ],
			spout[spout$VClass == "owc", ],
			spout[spout$VClass == "aw", ]
		)->back
	
		rbind(spout[spout$VClass == "iyr", ],
			spout[spout$VClass == "eyr", ],
			spout[spout$VClass == "aeh", ]
		)->frontin
	
		rbind(spout[spout$VClass == "uwr", ],
			spout[spout$VClass == "owr", ],
			spout[spout$VClass == "ohr", ],
			spout[spout$VClass == "oh", ],
			spout[spout$VClass == "ahr", ],
			spout[spout$VClass == "ah", ]
		)->backin	
		
		polygon(short$F2, short$F1)
		lines(front$F2,front$F1, col = "red")
		lines(back$F2, back$F1, col = "blue")
		lines(frontin$F2, frontin$F1, lty = 2, col = "red")
		lines(backin$F2, backin$F1, lty = 2, col = "blue")	
		
		rbind(short, front, back, frontin, backin)->pted
		text(pted$F2, pted$F1, pted$VClass, cex = .75)
		dev.off()
		print(paste(i, "Plotted and Saved", sep = " "))
}
}

reformat <- function(tels){
	nrow(tels)->r
	as.matrix(tels)->tels

	rbind(
		tels[,c(1:15,16:17)],
		tels[,c(1:15,18:19)],
		tels[,c(1:15,20:21)],
		tels[,c(1:15,22:23)],
		tels[,c(1:15,24:25)],
		tels[,c(1:15,27:28)],
		tels[,c(1:15,29:30)],
		tels[,c(1:15,31:32)],
		tels[,c(1:15,33:34)],
		tels[,c(1:15,36:37)],
		tels[,c(1:15,38:39)],
		tels[,c(1:15,40:41)],
		tels[,c(1:15,43:44)],
		tels[,c(1:15,45:46)],
		tels[,c(1:15,47:48)],
		tels[,c(1:15,49:50)],
		tels[,c(1:15,51:52)],
		tels[,c(1:15,53:54)],
		tels[,c(1:15,55:56)],
		tels[,c(1:15,57:58)],
		tels[,c(1:15,59:60)],
		tels[,c(1:15,61:62)],
		tels[,c(1:15,64:65)],
		tels[,c(1:15,66:67)])->tels

	data.frame(tels)->tels
	tels$VClass = c(
		rep("i",r),
		rep("e",r),
		rep("ae",r), 
		rep("o",r), 
		rep("uh",r), 
		rep("u",r), 
		rep("iyc",r), 
		rep("eyc",r), 
		rep("ayv",r), 
		rep("ay0",r), 
		rep("oy",r), 
		rep("iw",r), 
		rep("uwc",r), 
		rep("uwf",r), 
		rep("owc",r), 
		rep("aw",r), 
		rep("aeh",r), 
		rep("ah",r), 
		rep("oh",r), 
		rep("eyr",r), 
		rep("ahr",r), 
		rep("ohr",r), 
		rep("owr",r), 
		rep("uwr",r)
	)
	colnames(tels)[16:17] <- c("F1","F2")
	as.numeric(as.character(tels$F1))->tels$F1
	as.numeric(as.character(tels$F2))->tels$F2
	as.factor(tels$VClass)->tels$VClass
	return(tels)
}

spcom.format <-function(data){
	sub(" ","_",data$SPCOM)->data$SPCOM
	sub("\\.","",data$SPCOM)->data$SPCOM
	sub("\\'","",data$SPCOM)->data$SPCOM
	data[data$SPCOM == "Atlantic_Ci",]$SPCOM = rep("Atlantic_City", nrow(data[data$SPCOM == "Atlantic_Ci",]))
	data[data$SPCOM == "Cedar_Rapid",]$SPCOM = rep("Cedar_Rapids", nrow(data[data$SPCOM == "Cedar_Rapid",]))
	data[data$SPCOM == "Col_Spring",]$SPCOM = rep("Colorado_Springs", nrow(data[data$SPCOM == "Col_Spring",]))
	data[data$SPCOM == "Corpus_Chri",]$SPCOM = rep("Corpus_Christi", nrow(data[data$SPCOM == "Corpus_Chri",]))
	data[data$SPCOM == "Fayettevill",]$SPCOM = rep("Fayetteville", nrow(data[data$SPCOM == "Fayettevill",]))
	data[data$SPCOM == "Ft_Wayne",]$SPCOM = rep("Fort_Wayne", nrow(data[data$SPCOM == "Ft_Wayne",]))
	data[data$SPCOM == "Grand_Islan",]$SPCOM = rep("Grand_Island", nrow(data[data$SPCOM == "Grand_Islan",]))
	data[data$SPCOM == "Grand_Rapid",]$SPCOM = rep("Grand_Rapids", nrow(data[data$SPCOM == "Grand_Rapid",]))
	data[data$SPCOM == "Indianapoli",]$SPCOM = rep("Indianapolis", nrow(data[data$SPCOM == "Indianapoli",]))
	data[data$SPCOM == "Jacksonvill" ,]$SPCOM = rep("Jacksonville" , nrow(data[data$SPCOM == "Jacksonvill" ,]))
	data[data$SPCOM == "Jersey_Cit",]$SPCOM = rep("Jersey_City", nrow(data[data$SPCOM == "Jersey_Cit",]))
	data[data$SPCOM == "New_Brunsw",]$SPCOM = rep("New_Brunswick", nrow(data[data$SPCOM == "New_Brunsw",]))
	data[data$SPCOM == "Oklahoma_Ci",]$SPCOM = rep("Oklahoma_City", nrow(data[data$SPCOM == "Oklahoma_Ci",]))
	data[data$SPCOM == "Philadelphi",]$SPCOM = rep("Philadelphia", nrow(data[data$SPCOM == "Philadelphi",]))
	data[data$SPCOM == "Salt_Lake_C",]$SPCOM = rep("Salt_Lake_City", nrow(data[data$SPCOM == "Salt_Lake_C",]))
	data[data$SPCOM == "Sault_Saint",]$SPCOM = rep("Sault_Ste_Marie", nrow(data[data$SPCOM == "Sault_Saint",]))
	data[data$SPCOM == "South_Hadle",]$SPCOM = rep("South_Hadley", nrow(data[data$SPCOM == "South_Hadle",]))
	data[data$SPCOM == "State_Colle",]$SPCOM = rep("State_College", nrow(data[data$SPCOM == "State_Colle",]))
	data[data$SPCOM == "Stevens_Poi",]$SPCOM = rep("Stevens_Point", nrow(data[data$SPCOM == "Stevens_Poi",]))
	data[data$SPCOM == "WashingtonD",]$SPCOM = rep("Washington_DC", nrow(data[data$SPCOM == "WashingtonD",]))
	data[data$SPCOM == "Williamspor",]$SPCOM = rep("Williamsport", nrow(data[data$SPCOM == "Williamspor",]))
	data[data$SPCOM == "Winston-Sal",]$SPCOM = rep("Winston-Salem", nrow(data[data$SPCOM == "Winston-Sal",]))

	as.factor(data$SPCOM) -> data$SPCOM
	return(data)
}


get.packages <- function(){
	if (!exists("drop.levels")){
		try1 <- try(require(gdata),silent=T)
		if (try1==F|"try-error"%in%class(try1)){
			cat(sep="\n","Installing gdata package...")
			install.packages("gdata",dependencies=T)
			try1 <- try(require(gdata),silent=T)
		}
	}
}
