
########### eys.analysis
eys.stable <- subset(eys, Base == Coda)

eys.stable.coda.mean <- ddply(eys.stable, .(File, DOB, DOB0, Decade, Sex, Age, Year, Coda),
                              summarise, 
                              Diag = mean(Diag), .progress = "text")
                              

ggplot(eys.stable.coda.mean, aes(DOB, Diag, color = Coda)) + 
            geom_point() +
            stat_smooth()

eys.stable.coda.mean.c <- cast(eys.stable.coda.mean, 
                               File + DOB + Decade ~ Coda, 
                               value = "Diag")

ggplot(eys.stable.coda.mean.c, aes(closed-open, closed-final)) + 
            geom_point() +
            geom_abline() +
            facet_wrap(~Decade) + 
            coord_fixed()


## Statistical tests
library(lme4)

coda.moder1 <- lmer(Diag ~ I(DOB0/10)*Coda + (Coda | File), data = eys.stable)
coda.moder2 <- lmer(Diag ~ I(DOB0/10)*Coda + (Coda | File), data = eys.stable.coda.mean)
coda.mod <- lm(Diag ~ I(DOB0/10)*Coda, data = eys.stable.coda.mean)


## looking at manner of articulation
eys.closed <- subset(eys.stable, Coda %in% c("closed","open"))
eys.closed <- transform(eys.closed, Manner = as.factor(as.character(Manner)))
eys.closed <- subset(eys.closed, !is.na(Manner))

eys.closed$Manner <- relevel(eys.closed$Manner, c("stop")


eys.manner.mean <- ddply(eys.closed, .(File, DOB, Decade, DOB0, Manner), 
                    summarise,
                    Diag = mean(Diag))

ggplot(eys.manner.mean, aes(DOB, Diag, color = Manner)) +
          geom_point() + 
          stat_smooth()


## Statistics

manner.moder1 <- lmer(Diag ~ I(DOB0/10)*Manner + (Manner | File), data = eys.closed)
maner.moder2 <- lmer(Diag ~ I(DOB0/10)*Manner + (Manner | File), data = eys.manner.mean)

## lateral is clearly very different


eys.final <- subset(eys, Base == "final")

cast(eys.final, Coda ~ Morph)
##     Coda  NULL compound contract deriv inflect
## 1 closed     1        0      356    31    1222
## 2  final 19283        0        0     0       0
## 3 hiatus     0        1        5    30     530
## 4   open     0      753        0    12       0

eys.final <- subset(eys.final, Morph %in% c("NULL","inflect"))
eys.final <- subset(eys.final, Word != "A")

eys.final$Context <- NA
eys.final$Context[with(eys.final, grep("D$", Word))] <- "-ed"
eys.final$Context[with(eys.final, grep("S.?$", Word))] <- "-s"
eys.final$Context[with(eys.final, grep("IN.?$", Word))] <- "-ing"
eys.final$Context[is.na(eys.final$Context)] <- "final"

cast(eys.final, Morph ~ Context, value = "Diag")

eys.final <- subset(eys.final, !(Morph == "NULL" & Context == "-s"))

eys.final$Context <- as.factor(eys.final$Context)

eys.final$Context <- relevel(eys.final$Context, "final")

eys.final.mean <- ddply(eys.final, .(File, DOB, DOB0, Decade, Context),
                        summarise,
                        Diag = mean(Diag))


ggplot(eys.final.mean, aes(DOB, Diag, color = Context)) + 
              geom_point() + 
              stat_smooth()


final.moder1 <- lmer(Diag ~ I(DOB0/10)*Context + (Context | File), data = eys.final)
final.moder2 <- lmer(Diag ~ I(DOB0/10)*Context + (Context | File), data = eys.final.mean)
final.mod <- lm(Diag ~ I(DOB0/10)*Context, data = eys.final.mean)


### final dataset for comparison

eys.compare <- subset(eys, Morph %in% c("inflect","NULL"))
eys.compare$Context <- eys.compare$Coda
eys.compare$Context <- as.character(eys.compare$Context)
eys.compare$Context[eys.compare$Manner == "lateral"] <- "/l/"
eys.compare$Context <- gsub("open","closed",eys.compare$Context)

eys.compare$Context <- as.factor(eys.compare$Context)
eys.compare$Context <- relevel(eys.compare$Context, c("final", "/l/", "hiatus","closed"))

eys.compare.mean <- ddply(eys.compare, .(File, DOB, DOB0, Decade, Year, Context),
                          summarise,
                          Diag = mean(Diag))



ggplot(eys.compare.mean, aes(DOB, Diag, color = Context)) + 
        stat_smooth()


context.moder1 <- lmer(Diag ~ I(DOB0/10) * Context + (Context | File), data = eys.compare)




slope.levels <- levels(eys.compare$Context)

## This is a one-off, single use function
get.mer.slopes <- function(mer, slope.levels){
    fixefs <- fixef(mer)
    slopes <- c(0, fixefs[6:8]) + fixefs[2]
    return(data.frame(Context = slope.levels, coef = slopes))
}



## Compare results of a sociolinguistic study to a lab study
eys.compare.mean <- transform(eys.compare.mean, StudyYear = Decade + 18)
ggplot(eys.compare.mean, aes(Context, Diag, fill = Context)) + 
            geom_boxplot() + 
            facet_wrap(~StudyYear)


recent.compare.mean <- subset(eys.compare.mean, DOB >= 1980)
recent.compare.mean <- transform(recent.compare.mean, Year.2 = floor(DOB/2)*2)
recent.compare.mean <- transform(recent.compare.mean, StudyYear = Year.2 + 18)

ggplot(recent.compare.mean, aes(Context, Diag, color = Context)) + 
      geom_line(aes(group = File), color = "grey") +
      geom_point()  + 
      facet_wrap(~StudyYear)+
      theme_bw()

mean(ddply(eys.compare, .(Year), summarise, range = diff(range(Age)))$range)
#54

out <- vector(mod = "list", length = length(0:(max(eys.compare$DOB0)-50)))



for(i in 0:(max(eys.compare$DOB0)-50)){
  df <- subset(eys.compare, DOB0 >= i & DOB0 < i+50 )
  mod <- lmer(Diag ~ I(DOB0/10)*Context + (Context | File), data = df)
  slopes <- get.mer.slopes(mod, levels(df$Context))
  slopes$Iter <- i
  out[[i+1]] <- slopes
  cat(i)
  cat(" ")
}

n.out <- vector(mod = "list", length = length(0:(max(eys.compare$DOB0)-50)))
for(i in 0:(max(eys.compare$DOB0)-50)){
  df <- subset(eys.compare, DOB0 >= i & DOB0 < i+50 )
  n.out[[i+1]] <- data.frame(Iter = i, N = length(unique(df$File)) )
}

ldply(out, function(x)x)->out.df
ldply(n.out, function(x)x)->n.out.df
merge(out.df, n.out.df)->out.df

out.df$Context <- relevel(out.df$Context,  c("final", "/l/", "hiatus","closed"))

ggplot(out.df, aes((Iter+1888) + 68 , coef, color = Context)) +
        geom_hline(y = 0)+
        geom_line()+ 
        xlab("Study Year") 



################
## ays analysis


ay0 <- subset(all_philly, VClass %in% c("ay0"))

ay0.mean <- ddply(ay0, c(speaker.id, "VClass"), summarise, F1 = mean(F1.n), F1.sd = sd(F1.n))


