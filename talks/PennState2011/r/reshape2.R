## International data on Gender Disparity
## Source: Guardian Data blog
##    http://www.guardian.co.uk/news/datablog/2009/mar/10/gender-educationsgendergap
##  UNESCO
##    http://hdr.undp.org/en/statistics/data/

gender <- read.delim("talks/PennState2011/r/data/gender_equality.txt")

head(gender)

## Column 1 is an ID variable for Country. 
## Notice how the various measure variables (Life exptectancy, Literacy, Educational Enrolment, etc.)
## and gender are combined together to structure the remaining columns.

## install.packages("reshape2")
library(resape2)

gender.m <- melt(gender, id = 1)
# or, equivalently
# melt(gender, id = "Country")

head(gender.m)

## There is a convenience function, colsplit(), which will allow us to split up the measures
## from gender.

gender.m <- cbind(gender.m,
                colsplit(gender.m$variable, "_", names = c("Measure","Gender"))
              )

## If you want to compare measures:

gender.c <- dcast(gender.m, Country + Gender ~ Measure)
head(gender.c)

## This is plotting code, not covered in the tutorial.
library(ggplot2)
ggplot(gender.c, aes(Edu, log(Income), color = Gender)) + 
  geom_point() +
  stat_smooth(method = "lm")



## If you want to compare genders

gender.c <- dcast(gender.m, Country + Measure ~ Gender)
head(gender.c)

ggplot(gender.c, aes(Male, Female)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~Measure, scales = "free")


## You can also use dcast() to produce some summaries.

## Simple number of observations
dcast(gender.m, Measure ~ Gender)

## Mean values
dcast(gender.m, Measure ~ Gender, fun = mean, na.rm = T)

## Standard Deviation
dcast(gender.m, Measure ~ Gender, fun = sd, na.rm = T)
