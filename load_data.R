## Load the data
### download from https://www.google.com/fusiontables/DataSource?snapid=S4035208e94
philly <- read.csv("Murders in Philadelphia County 1988 - 2011.csv", na.strings = "")

## Process the Hour of the day
philly$half <- NA
philly$half[grepl("[aA]", philly$time, )] <- "AM"
philly$half[grepl("[pP]", philly$time, )] <- "PM"
philly$hour <- gsub("[aApP][mM]?", "", philly$time)
philly$hour <- as.numeric(philly$hour)
philly$hour[philly$hour > 12] <- NA
philly$hour[is.na(philly$half)] <- NA
philly$hour[philly$hour == 12 & !is.na(philly$hour)] <- 0
philly$hour[philly$half == "PM" 
						& !is.na(philly$half)] <- philly$hour[philly$half == "PM" & 
																									!is.na(philly$half)] + 12

## Hour of day centered with midnight as 0
philly.hour <- subset(philly, !is.na(hour))
philly.hour$hour2 <- philly.hour$hour
philly.hour$hour2[philly.hour$half == "PM"] <- philly.hour$hour[philly.hour$half == "PM"] - 24

## Fill in missing hours as midnight
philly$hour3 <- philly$hour
philly$hour3[is.na(philly$hour)] <- 0
philly$date <- paste(philly$date, philly$hour3)


## Get various date information
dates <- as.POSIXlt(philly$date, format = "%m/%d/%y %H")
philly$date <- as.Date(dates)
philly$month <- as.factor(month.abb[dates$mon+1])
philly$month <- reorder(philly$month, dates$mon, min)
philly$year <- dates$year + 1900
philly$monthn <- dates$mon + 1

## Month date indicates just the month in Date format
philly$month.date <- as.POSIXct(paste(dates$mon + 1,1, dates$year + 1900, sep = "/"), format = "%m/%d/%Y")
philly$month.date <- as.Date(philly$month.date)



## Weekdays
philly$WDay <- weekdays(dates)
