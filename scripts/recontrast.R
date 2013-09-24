###########
#	recontrast.R
#	Copyright 2009 Josef Fruehwald
#   This program is free software: you can redistribute it and/or modify
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
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
############


recontrast<-function(data,type = "sum"){
	data.type <-class(data)
	if(data.type == "factor"&!is.ordered(data)&nlevels(data)>1&nlevels(data)<1000){
		if(type == "sum"){
			contrasts(data)<-contr.sum(levels(data))
			colnames(contrasts(data))<-levels(data)[-nlevels(data)]
		}else if(type == "treatment"){
			contrasts(data)<-contr.treatment(levels(data))
		}
	}else if(data.type == "data.frame"){
		for(i in 1:ncol(data)){
			if(is.factor(data[,i]) & !is.ordered(data[,i])&nlevels(data[,i])>1&nlevels(data[,i])<1000){
				if(type == "sum"){
					contrasts(data[,i])<-contr.sum(levels(data[,i]))
					colnames(contrasts(data[,i]))<-levels(data[,i])[-nlevels(data[,i])]
				}else if(type == "treatment"){
					contrasts(data[,i])<-contr.treatment(levels(data[,i]))
				}
			}
		}
	}
return(data)
}
