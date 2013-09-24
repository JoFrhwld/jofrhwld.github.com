
## @knitr unnamed-chunk-1
    library(ggplot2)
    library(reshape2)
    library(plyr)
    library(mgcv)
    library(tikzDevice)
    library(moments)
    library(gtools)
    library(grid)
    library(boot)
    library(RColorBrewer)


## @knitr unnamed-chunk-2
    options(tikzLatexPackages = c(getOption("tikzLatexPackages"),
                                  "\\usepackage{tipa}",
                                  "\\usepackage{booktabs}",
                                  "\\usepackage{qtree}"),
            width=11)


## @knitr load-data
    contextFILES <- Sys.glob("~/Documents/Classes/FAAV_Data/PH*txt")
    
    read.context <- function(file, ...){
        require(stringr)
        filename = rev(unlist(str_split(file, "/")))[1]
        data <- read.delim(file, ...)
        data <- cbind(File = filename, data)
        return(data)
    } 
    
    data <- ldply(contextFILES, 
                  read.context, 
                  header = F, 
                  colClasses = "character", 
                  .progress = "text")

    colnames(data) <- c("File", "Name","Age",
                        "Sex","Eth", "Edu",
                        "Street","Year", "F1",
                        "F2", "F3", "VClass",
                        "Manner", "Place", "Voice",
                        "PreSeg", "FolSeq", "Stress_Dur",
                        "Info", "Word", "Time",
                        "Context", "PreSegTrans", "FolSegTrans",
                        "Fole2SegTrans", "Trans", "FolTrans")


## @knitr format-data

    data$F1 <- as.numeric(data$F1)
    data$F2 <- as.numeric(data$F2)
    data$F3 <- as.numeric(data$F3)
    data$Year <- as.numeric(data$Year)
    data$Age <- as.numeric(data$Age)
    
    strdur <- colsplit(data$Stress_Dur, split = "\\.", names = c("Stress","Dur_msec"))
    data <- cbind(data, strdur)
    
    
    
    data <- ddply(data, .(File), 
                  transform, F1.n = rescaler(F1), F2.n = rescaler(F2), .progress = "text")
    data <- transform(data, DOB = Year-Age)
    data <- transform(data, DOB0 = DOB-min(DOB), Decade = (DOB-min(DOB))/10)
    
    eth <- subset(data, Eth %in% c("a","h"))
    data <- subset(data, !Eth %in% c("a","h"))



## @knitr fake-load
    load("~/Documents/Classes/FAAV_Data/basic_context.Rdata")


## @knitr ay-processing
    ays <- load.vowels(c("ay","ay0"), path)
    ays2 <- subset(ays, 
                   !Word %in% c("LIKE","RIGHT", "I'D", "I'M","I'LL", "I'VE", "MY","I"))
    ays2$Voice <- as.character(ays2$FolSegTrans)
    ays2$Voice[grep("S|F|K|P|HH|CH|SH|TH|T",ays2$FolSegTrans)] <- "voiceless"
    ays2$Voice[grep("V|Z|B|G|JH|ZH|DH|D",ays2$FolSegTrans)] <- "voiced"
    ays2$Voice[grep("N|M",ays2$FolSegTrans)] <- "nasal"
    ays2$Voice[grep("L|^R",ays2$FolSegTrans)] <- "liquid"
    ays2$Voice[grep("^W|^Y",ays2$FolSegTrans)] <- "glide"
    ays2$Voice[grep("[AEIOU]",ays2$FolSegTrans)] <- "vowel"
    
    ays2 <- subset(ays2, !FolSegTrans %in% c("ns","lg","br"))
    ays2 <- transform(ays2, DOB = Year - Age)
    ays2 <- transform(ays2, DOB0 = DOB-min(DOB), Decade = (DOB-min(DOB))/10)



## @knitr ellegard-data
    do<-read.csv("~/Documents/Classes/Fall_09/Syntax/Rellegard.txt")
    colnames(do)<- c("Do", "Polarity", "Type", "Subtype", "Transitivity", 
                     "ElseCond", "Verb", "Subject", "Order", "NotPlace", "Text", 
                     "TextCh", "Date")
    levels(do$Polarity) <- c("Affirmative","NegNoNot","NegNot")
    levels(do$Type) <- c("ImperativeComma","Declarative","Imperative","Question")
    do$Date <- do$Date + 1000

    do<-do[!is.na(do$Do),]
    do$DoN <- (as.numeric(do$Do)*-1)+2 ## this recodes Do as 0 and 1
    
    neg.dec <- subset(do, Polarity=="NegNot", Type = "Declarative")


## @knitr ellegard-text-means
    neg.dec.means <- ddply(neg.dec, .(TextCh, Date), summarise,
                           prop = mean(DoN), N = length(DoN))


## @knitr ellegard-model
    elle.mod <- gamm(DoN~s(Date, bs = "cs"), random=list(TextCh = ~1), data = neg.dec, 
                     link=logit)
    pred <- data.frame(Date = min(neg.dec$Date):max(neg.dec$Date))
    pred$`T-to-V` <- predict(elle.mod$gam, newdata = pred)
    pred$`V-to-T` <- 1-pred$`T-to-V`
    
    pred.m <- melt(pred, id = 1)


## @knitr basic-ay0-mean
    ay0 <- subset(ays2, Voice == "voiceless")
    ay0.mod <- gamm(-F1.n~s(DOB, bs = "cs"), random=list(File = ~1), 
                    data = ay0)
    
    pred.ay <- data.frame(DOB = min(ay0$DOB):max(ay0$DOB))
    pred.ay$F1 <- predict(ay0.mod$gam, newdata = pred.ay)
    
    ay0.mean <- ddply(ay0, .(File, DOB, Sex), summarise,
                      F1 = mean(-F1.n),N=length(F1))
    
    ay0.mean <- subset(ay0.mean, F1 > -2.5)
        


## @knitr elle-plot1
    ggplot(pred, aes(Date, `T-to-V`))+
        geom_point(data = neg.dec.means, aes(y=prop,size = N), color = "grey40")+
        geom_line(size = 2)+
        scale_area()+
        ylab("Proportion T-to-V")+
        ylim(0,1)+
        theme_bw()+
        opts(legend.position="none", title = "{\\it do}-support in Negative Declaratives")
    
    ggplot(pred.ay, aes(DOB, F1))+
        geom_point(data=ay0.mean, aes(size = N), color = "grey40")+
        geom_line(size = 2)+
        scale_area()+
        theme_bw()+
        opts(legend.position="none",title ="Pre-voiceless /ay/ Raising")+
        xlab("Date of Birth")


## @knitr ani-setup2
    decades <- (140:171)*10    


## @knitr elle-plot-ani

    for(x in decades){
        dfnew <- subset(pred.m, Date <= x)
        
        print(ggplot(dfnew, aes(Date, value, fill=variable))+
            geom_area(color = "black", size = 2)+
            ylim(0,1)+
            xlim(1400,1710)+
            theme_bw()+
            annotate(x=1460, y=0.95, geom = "text", 
                     label = "\\tiny{\\Tree [.TP [ ] [ [.T T V$_i$ ] [.NegP Neg [.VP [. ] [ t$_i$ [ ] ] ] ] ] ]}")+
             annotate(x=1670, y=0.3, geom = "text", 
                      label = "\\tiny{\\Tree [.TP [ ] [ [.T do ] [.NegP Neg [.VP [. ] [ V [ ] ] ] ] ] ]}")+
             opts(legend.position="none", 
                  title = "{\\it do}-support in Negative Declaratives")+
             ylab("Proportion Use")+
             scale_fill_brewer(palette="Pastel1"))}


## @knitr ay-ani-setup
    ay.r5 <- floor(range(ay0$DOB)/5)
    ay.5ade <- ((ay.r5[1]:ay.r5[2]) * 5)[-1]


## @knitr ay-plot-ani
    for(x in ay.5ade){
        dfnew <- subset(pred.ay, DOB <=x)
        ay0.mean$diff <- abs(ay0.mean$DOB-x)+1
        point.means <- subset(ay0.mean, DOB <=x)
        
        print(ggplot(dfnew, aes(DOB, F1))+
            geom_point(data=point.means, aes(size = N, alpha = 1/(diff)))+
            geom_line(size = 2)+
            scale_area()+
            theme_bw()+
            opts(legend.position="none",title ="Pre-voiceless /ay/ Raising")+
            xlab("Date of Birth")+
            ylim(min(c(pred.ay$F1, ay0.mean$F1)), 
                 max(c(pred.ay$F1, ay0.mean$F1)))+
            xlim(min(pred.ay$DOB), max(pred.ay$DOB))+
            scale_alpha(range=c(0,1),trans="log",limits=c(1/103,1))
             )
    }


## @knitr aw-setup
    aw <- load.vowels("aw",path)
    aw$Nasal <- "other"
    aw$Nasal[aw$Context == "Initial" & 
             aw$FolSegTrans %in% c("N","M","NG")] <- "following"
    aw$Nasal[aw$Context %in% c("Internal") & 
            aw$FolSegTrans %in% c("M", "N", "NG") &
            !aw$PreSegTrans %in% c("M","N","NG")] <- "following"
    aw$Nasal[aw$Context %in% c("Internal") & 
            aw$FolSegTrans %in% c("M", "N", "NG") &
            aw$PreSegTrans %in% c("M","N","NG")] <- "sandwich"
    aw$Nasal[aw$Context %in% c("Internal") & 
            aw$PreSegTrans %in% c("M", "N", "NG")&
            !aw$FolSegTrans %in% c("M","N","NG")] <- "preceding"
    aw$Nasal[aw$Context %in% c("Final") & 
            aw$PreSegTrans %in% c("M", "N", "NG")] <- "preceding"


    aw.means <- ddply(aw, .(File, Sex, DOB, Nasal), summarise,
                      F1 = mean(F1.n), F2 = mean(F2.n), Diag = mean(F2.n-F1.n),
                      N = length(F1.n))
    aw.means <- subset(aw.means, Nasal !="sandwich")


## @knitr unnamed-chunk-3
 ggplot(aw.means, aes(DOB, Diag, color = Nasal)) + 
     geom_point(aes(size = N)) + 
     scale_area(guide = F) + 
     stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"), size = 1.7, 
                 color = "black",se = F, aes(group = Nasal, weight = log1p(N)))+
     stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"), size = 1.5,
                 aes(weight = log1p(N)))+
     theme_bw()+
     scale_color_brewer(palette = "Set2", 
                        breaks = c("following", "preceding","other"),
                        limits = c("following", "preceding","other") )+
     xlab("Date of Birth")


## @knitr aw-ani-setup
    aw <- subset(aw, Nasal!="sandwich")
    aw.f1.mods <- dlply(aw,.(Nasal), function(df){
                     gamm(F1.n ~ s(DOB, bs = "cs"), 
                          random = list(File = ~1), 
                          data = df)})

    aw.f2.mods <- dlply(aw,.(Nasal), function(df){
        gamm(F2.n ~ s(DOB, bs = "cs"), 
             random = list(File = ~1), 
             data = df)})
    
    aw.pred <- expand.grid(DOB = 1888:1991)
    
    aw.f1.pred <- ldply(aw.f1.mods, function(x,df){
                        df$F1 <- predict(x$gam, newdata = df)
                        return(df)
                },df = aw.pred)
    aw.f2.pred <- ldply(aw.f2.mods, function(x,df){
                        df$F2 <- predict(x$gam, newdata = df)
                        return(df)
                },df = aw.pred)
    
    aw.preds <- merge(aw.f1.pred, aw.f2.pred)


## @knitr aw-ani-setup2
    aw.2ade <- sort(unique(floor(aw.means$DOB/2)*2))[-1] 


## @knitr aw-ani-new
    

    for(x in aw.2ade){
        dfnew <- subset(aw.preds, DOB <=x)
        
        date.df <- data.frame(x = x)
        d <- ggplot(date.df, aes(x=x, y = "Date")) + 
            geom_segment(xend=1888, x = 1991, yend=1, size = 1) + 
            xlim(1888,1999) + 
            geom_point(size = 3) + 
            geom_text(aes(label = x, y = 1.3))+
            theme_bw()+
            opts(axis.line = theme_blank(),
                 axis.text.x = theme_blank(),
                 axis.text.y=theme_blank(),
                 axis.ticks = theme_blank(),
                 axis.title.x = theme_blank(),
                 axis.title.y = theme_blank(),
                 panel.background = theme_blank(),
                 panel.border = theme_blank(),
                 panel.grid.major = theme_blank(),
                 panel.grid.minor = theme_blank())
        
        g <- ggplotGrob(d)
        print(ggplot(dfnew, aes(-F2, -F1, color = Nasal))+
            geom_hline(y=0, color = "grey")+
            geom_vline(x=0, color = "grey")+
            geom_path(arrow = arrow())+
            #geom_point(data = point.means, aes(alpha = 1/diff),limits=c(1/103,1))+
            #scale_alpha(range=c(0,1), guide = F)+
            xlim(-0.5,0.7)+
            ylim(-1.7,0)+
            theme_bw()+
            annotation_custom(grob = g, xmin=-0.6, xmax = 0.8, ymin=-1.9, ymax = -1.5)+
            scale_color_brewer(palette = "Set2", 
                               breaks = c("following", "preceding","other"),
                               limits = c("following", "preceding","other") ))
    }


## @knitr ay-cond-setup
    ay.comp <- subset(ays2, Voice %in% c("voiced","voiceless"))
    ay.comp.mean <- ddply(ay.comp, .(File, DOB, Sex, Voice), summarise,
                          F1 = mean(F1.n), F2 = mean(F2.n), N = length(F1.n))
    


## @knitr ay-cond
 ggplot(ay.comp.mean, aes(DOB, -F1, color = Voice)) + 
     geom_point(aes(size = N)) + 
     scale_area(guide = F) + 
     stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"), size = 1.7, 
                 color = "black",se = F, aes(group = Voice, weight = log1p(N)))+
     stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"), size = 1.5,
                 aes(weight = log1p(N)))+
     theme_bw()+
     scale_color_brewer(palette = "Set1", limits = c("voiceless","voiced"))+
     xlab("Date of Birth")


## @knitr ay-cond-ani-setup
    ay.f1.mods <- dlply(ay.comp, .(Voice), function(df){
                    gamm(F1.n ~ s(DOB, bs = "cs"), random = list(File= ~1), data = df)
            })
    ay.f2.mods <- dlply(ay.comp, .(Voice), function(df){
                    gamm(F2.n ~ s(DOB, bs = "cs"), random = list(File= ~1), data = df)
            })
    
    ay.pred <- expand.grid(DOB = 1888:1991)
    
    ay.f1.pred <- ldply(ay.f1.mods, function(x,df){
                        df$F1 <- predict(x$gam, newdata = df)
                        return(df)
                },df = ay.pred)
    ay.f2.pred <- ldply(ay.f2.mods, function(x,df){
                        df$F2 <- predict(x$gam, newdata = df)
                        return(df)
                },df = ay.pred)
    
    ay.preds <- merge(ay.f1.pred, ay.f2.pred)


## @knitr ay-ani-new
    

    for(x in aw.2ade){
        dfnew <- subset(ay.preds, DOB <=x)
                        
        date.df <- data.frame(x = x)
        d <- ggplot(date.df, aes(x=x, y = "Date")) + 
            geom_segment(xend=1888, x = 1991, yend=1, size = 1) + 
            xlim(1888,1999) + 
            geom_point(size = 3) + 
            geom_text(aes(label = x, y = 1.3))+
            theme_bw()+
            opts(axis.line = theme_blank(),
                 axis.text.x = theme_blank(),
                 axis.text.y=theme_blank(),
                 axis.ticks = theme_blank(),
                 axis.title.x = theme_blank(),
                 axis.title.y = theme_blank(),
                 panel.background = theme_blank(),
                 panel.border = theme_blank(),
                 panel.grid.major = theme_blank(),
                 panel.grid.minor = theme_blank())
        
        g <- ggplotGrob(d)
        print(ggplot(dfnew, aes(-F2, -F1, color = Voice))+
            geom_hline(y=0, color = "grey")+
            geom_vline(x=0, color = "grey")+
            geom_path(arrow = arrow())+
            #geom_point(data = point.means, aes(alpha = 1/diff),limits=c(1/103,1))+
            #scale_alpha(range=c(0,1), guide = F)+
            xlim(-0.5,0.7)+
            ylim(-1.7,0)+
            theme_bw()+
            annotation_custom(grob = g, xmin=-0.6, xmax = 0.8, ymin=-1.9, ymax = -1.5)+
            scale_color_brewer(palette = "Set1", limits = c("voiceless","voiced")))
    }


## @knitr ay-coding-setup
    ay.vc <- subset(ays2, 
                    Context %in% c("Internal","Initial") &
                        !grepl("[AEIOU]", FolSegTrans))



## @knitr flap-coding-internal
    internal.test <- with(ay.vc, paste("AY. ", FolSegTrans, " ", Fole2SegTrans, sep = ""))
    internal.tf <- rep(F, length(internal.test))
    for(i in 1:length(internal.test)){
        trans <- ay.vc$Trans[i]
        internal.tf[i] <- grepl(internal.test[i], trans) 
    }
    ay.vcx <- ay.vc[internal.tf,]
    ay.vcv <- subset(ay.vcx, grepl("[AEIOU]", Fole2SegTrans))
    ay.vcc <- subset(ay.vcx, !grepl("[AEIOU]", Fole2SegTrans))


## @knitr ay-coding-edge
    edge.test <- with(ay.vc, paste("AY. ", FolSegTrans,"$", sep = ""))
    edge.tf <- rep(F, length(edge.test))
    for(i in 1:length(edge.test)){
        trans <- ay.vc$Trans[i]
        edge.tf[i] <- grepl(edge.test[i], trans) 
    }
    ay.vc.x <- ay.vc[edge.tf,]
    ay.vc.v <- subset(ay.vc.x, grepl("[AEIOU]", Fole2SegTrans))
    ay.vc.c <- subset(ay.vc.x, !grepl("[AEIOU]", Fole2SegTrans))


## @knitr ay-coding-flaps
    ay.vcv.td <- subset(ay.vcv, FolSegTrans %in% c("T","D") & 
                        !grepl("1",  ay.vcv$Fole2SegTrans))
    #dput(sort(unique(ay.vcv.td$Word)))
    stopwords <- c("CIDER",
                   "ENLIGHTENMENT", 
                   "FIGHTIN'",
                   "FRIGHTEN", 
                   "FRIGHTENED", "FRIGHTENING",
                   "IDLE", "IDOL",
                   "LIGHTENER", 
                   "LIGHTIN'",
                   "SNEIDER", "SNYDER", 
                   "SNYDER'S", "SPIDER", "SPIDERMAN", "SPIDERS", "TIGHTEN", 
                   "TIGHTENS", 
                   "WRITIN'", 
                   "WRITINGS")  
    
    ay.vcv.td <- subset(ay.vcv.td, !Word %in% stopwords & Context != "Initial")
    ay.vcv.td$Context <- "internal flap"


## @knitr ay-coding-nonflap
    ay.vc.sp <- subset(ay.vc, Fole2SegTrans == "sp")
    ay.vc.sp.td <- subset(ay.vc.sp, FolSegTrans %in% c("T","D"))
    ay.vc.sp.td$Context <- "C#"


## @knitr ay-td-comp
    ay.td.comp <- rbind.fill(ay.vcv.td, ay.vc.sp.td)
    ay.td.comp.mean <- ddply(ay.td.comp, .(File, DOB, Context, FolSegTrans),
                             summarise,
                             N = length(F1.n),
                             F1 = mean(F1.n),
                             F2 = mean(F2.n),
                             Dur_msec = median(Dur_msec))
    ay.td.comp.mean$Context <- as.factor(ay.td.comp.mean$Context)
    ay.td.comp.mean$FolSegTrans <- as.factor(as.character(ay.td.comp.mean$FolSegTrans))



## @knitr ay-duration-plot
    ggplot(ay.td.comp.mean, aes(DOB, Dur_msec, color = FolSegTrans, linetype = Context))+
        stat_smooth(method = gam, 
                    formula = y ~ s(x, bs = "cs"), 
                    aes(weight=log1p(N), group = Context:FolSegTrans),
                    geom = "ribbon",
                    color = NA,
                    fill = "grey",
                    alpha = 0.5)+
        stat_smooth(method = gam, 
                    formula = y ~ s(x, bs = "cs"), 
                    aes(weight=log1p(N)),
                    geom = "line") +
        theme_bw()+
        expand_limits(y=0)+
        scale_color_brewer(name = "Following\nSegment", 
                           palette = "Set1",
                           limits = c("T","D"))+
        xlab("Date of Birth")+
        ylab("Duration (msec)")


## @knitr ay-flap-plot
    ggplot(ay.td.comp.mean, aes(DOB, -F1, color = FolSegTrans, linetype = Context))+
        geom_hline(y = 0, color = "darkgrey")+
        stat_smooth(method = gam, 
                    formula = y ~ s(x, bs = "cs"), 
                    aes(weight=log1p(N), group = Context:FolSegTrans),
                    geom = "ribbon",
                    color = NA,
                    fill = "grey",
                    alpha = 0.5)+
        stat_smooth(method = gam, 
                    formula = y ~ s(x, bs = "cs"), 
                    aes(weight=log1p(N)),
                    geom = "line") +
        theme_bw()+
        scale_color_brewer(name = "Following\nSegment", 
                           palette = "Set1",
                           limits = c("T","D"))+
        xlab("Date of Birth")


## @knitr flap-mods
    d.test.df <- subset(ay.td.comp, 
                        FolSegTrans == "D" & 
                            Context %in% c("C#","internal flap"))
    t.test.df <- subset(ay.td.comp, 
                        FolSegTrans == "T" & 
                            Context %in% c("C#","internal flap"))
    
    d.test.df <- transform(d.test.df, 
                           File = as.factor(as.character(File)),
                           Word = as.factor(as.character(Word)),
                           Context = as.factor(as.character(Context)))    
    t.test.df <- transform(t.test.df, 
                           File = as.factor(as.character(File)),
                           Word = as.factor(as.character(Word)),
                           Context = as.factor(as.character(Context)))    

    d.mod1 <- gamm(-F1.n ~ Context + s(DOB, by = Context, bs = "cs"),
                   data = d.test.df,
                   random = list(File = ~ 1, Word = ~ 1))
    t.mod1 <- gamm(-F1.n ~ Context + s(DOB, by = Context, bs = "cs"),
                   data = t.test.df,
                   random = list(File = ~ 1, Word = ~ 1))

    d.mod2 <- gamm(-F1.n ~ Context + s(DOB, bs = "cs"),
                data = d.test.df,
                random = list(File = ~ 1, Word = ~ 1)
              )

    t.mod2 <- gamm(-F1.n ~ Context + s(DOB, bs = "cs"),
                   data = t.test.df,
                   random = list(File = ~ 1, Word = ~ 1)
                   )
    
    d.mod3 <- gamm(-F1.n ~  s(DOB, bs = "cs"),
                   data = d.test.df,
                   random = list(File = ~ 1, Word = ~ 1)
                   )
    t.mod3 <- gamm(-F1.n ~  s(DOB, bs = "cs"),
                   data = t.test.df,
                   random = list(File = ~ 1, Word = ~ 1)
                   )
    
    
    
    t.aic <- AIC(t.mod1$lme, t.mod2$lme, t.mod3$lme)
    t.bic <- BIC(t.mod1$lme, t.mod2$lme, t.mod3$lme)
    
    d.aic <- AIC(d.mod1$lme, d.mod2$lme, d.mod3$lme)
    d.bic <- BIC(d.mod1$lme, d.mod2$lme, d.mod3$lme)
    


## @knitr gamm-fit
    pred.ay.gam <- expand.grid(DOB = 1888:1991, Context = c("C#", "internal flap"))
    pred.ay.gam$T_F1 <- predict(t.mod1$gam, newdata = pred.ay.gam)
    pred.ay.gam$T_SE <- predict(t.mod1$gam, newdata = pred.ay.gam, se = T)$se
    
    pred.ay.gam$D_F1 <- predict(d.mod1$gam, newdata = pred.ay.gam)
    pred.ay.gam$D_SE <- predict(d.mod1$gam, newdata = pred.ay.gam, se = T)$se
    
    pred.ay.gam.m <- melt(pred.ay.gam, id = 1:2)
    pred.ay.gam.m <- cbind(pred.ay.gam.m, colsplit(pred.ay.gam.m$variable, pattern="_", 
                                                   names = c("FolSegTrans","Measure")))
    pred.ay.gam.c <- dcast(pred.ay.gam.m, DOB + Context + FolSegTrans ~ Measure)


## @knitr unnamed-chunk-4
    ggplot(pred.ay.gam.c, aes(DOB, F1, color = FolSegTrans, linetype = Context))+
        geom_hline(y = 0, color = "darkgrey")+
        geom_ribbon(aes(ymax = F1 + (1.96 * SE), 
                        ymin = F1 - (1.96*SE), 
                        group = paste(Context, FolSegTrans)),
                    fill = "grey",
                    color = NA,
                    alpha = 0.3)+
        geom_line()+
        theme_bw()+
        scale_color_brewer(name = "Following\nSegment", 
                           palette = "Set1",
                           limits = c("T","D"))+
        xlab("Date of Birth")


## @knitr snyder-setup
    exceptional <- data.frame(Word = c("CIDER","IDLE", "IDOL",
                                       "SNEIDER", "SNYDER", 
                                       "SNYDER'S", "SPIDER", "SPIDERMAN", 
                                       "SPIDERS", "FRIDAY","FRIDAYS"),
                              Stem = c("CIDER","IDLE", "IDOL",
                                       "SNYDER", "SNYDER", 
                                       "SNYDER", "SPIDER", "SPIDER", 
                                       "SPIDER", "FRIDAY","FRIDAY"))
    
    ay.excep <- subset(ays2, Word %in% exceptional$Word)
    ay.excep <- merge(ay.excep, exceptional)
    ay.excep <- subset(ay.excep, Stem %in% c("FRIDAY","SNYDER","SPIDER"))
    ay.excep$Class <- as.character(ay.excep$Stem)
    ay.excep$Class[ay.excep$Stem %in% c("CIDER","SNYDER","SPIDER")] <- "SNYDER"
    
    ay.excep.comp <- subset(ays2, FolSegTrans %in% c("T","D") & Context == "Internal" & 
                                  File %in% ay.excep$File & !(Word %in% ay.excep$Word))
    
    ay.excep.mean <- ddply(ay.excep, .(File, DOB, Class), 
                           summarise, 
                           F1 = mean(F1.n), N = length(F1.n))

    ay.excep.comp.mean <- dcast(ay.excep.comp, 
                                File + DOB ~ FolSegTrans, 
                                value.var = "F1.n", 
                                fun = mean)
    ay.excep.mean <- merge(ay.excep.mean, ay.excep.comp.mean)
    ay.excep.mean <- transform(ay.excep.mean, F1 = -F1, D = -D, `T` = -`T`)
    
    ay.excep.mean <- transform(ay.excep.mean, Ratio = (F1 - D) / (`T` - D), diff = `T` - D)
    ay.excep.mean2 <- subset(ay.excep.mean, diff >0)


## @knitr snyder-plot
    pal <- brewer.pal(n=3, name = "Set1")

    ggplot(subset(ay.excep.mean2, Class == "SNYDER"), aes(DOB, F1)) + 
        geom_hline(y = 0, color = "grey80")+
        geom_point(aes(size = N))+
        stat_smooth(aes(y = `T`,color = "/t/", linetype = "/t/"))+
        stat_smooth(aes(y = D, color = "/d/",linetype = "/d/")) + 
        stat_smooth(aes(weight = log1p(N), linetype = "lexical", color = "lexical"))+
        facet_wrap(~Class)+
        scale_area(guide = "none")+
        theme_bw()+
        scale_color_manual(name = "Context",values = c(pal[2:1], "black"))+
        scale_linetype_manual(name = "Context", values = c(1,1,2))+
        xlab("Date of Birth")+
        xlim(1889,1991)+
        ylim(-3, 1)
    


## @knitr more-flap-coding
    ay.vc.sp.td <- subset(ay.vc.sp, FolSegTrans %in% c("T","D"))
    ay.vc.v.td <- subset(ay.vc.v, FolSegTrans %in% c("T","D"))
    ay.vcc.td <- subset(ay.vcc, FolSegTrans %in% c("T","D"))
    ay.vc.c.td <- subset(ay.vc.c, FolSegTrans %in% c("T","D") & Context != "Initial")

    ay.vcv.td$Context <- "internal flap"
    ay.vc.v.td$Context <- "boundary flap"
    ay.vc.sp.td$Context <- "C#"
    ay.vcc.td$Context <- "vcc#"
    ay.vc.c.td$Context <- "vc#c"
    
    ay.td.comp <- rbind.fill(ay.vcv.td, ay.vc.v.td, 
                             ay.vc.sp.td, ay.vcc.td, 
                             ay.vc.c.td)
    


## @knitr ay-freq-frame
    context.freq <- dcast(ay.td.comp, PreSegTrans + FolSegTrans ~ Context)
    ay.vcv.td2 <- merge(ay.vcv.td, context.freq)
    
    ay.vcv.t <- subset(ay.vcv.td2, FolSegTrans == "T")
    ay.vcv.t <- transform(ay.vcv.t, MoreTrans = `C#`+`vc#c`+`vcc#` > (`internal flap` + `boundary flap`), 
                          Transparent =`C#`,
                          Opaque =  `internal flap`)
    ay.vcv.t <- transform(ay.vcv.t, Ratio = Transparent/Opaque)
    ay.vcv.t <- transform(ay.vcv.t, Ratio.c = cut(Ratio, breaks= c(0,1,3,Inf), include.lowest = T))    

    ay.vcv.t.mean <- ddply(ay.vcv.t, .(File, DOB, Ratio.c), summarise, 
                        F1 = mean(F1.n),
                        N = length(F1.n))



## @knitr analogy1
    pal = brewer.pal(7,"OrRd")
    
    ggplot(ay.vcv.t.mean, aes(DOB, -F1, color = Ratio.c)) +
        geom_hline(y = 0, color = "grey80")+
        geom_point(aes(size = N)) + 
        scale_area(guide = "none") + 
        ylim(-3,1) + 
        stat_smooth(method = gam, formula = y ~ s(x), aes(weight = log1p(N)), size = 1.25) +
        scale_color_manual(values = pal[c(3,5,7)],
                           name = "Ratio\nTransparent/Opaque",
                           breaks = levels(ay.vcv.t$Ratio.c),
                           labels = c("0 to 1x", "1 to 3x", "3+ x")
                           ) + 
                               theme_bw()


## @knitr frame-d-setup
    flap.context <- dcast(ay.vcv.td,  PreSegTrans ~ FolSegTrans)
    ay.vcv.td3 <- join(ay.vcv.td, flap.context)
    ay.vcv.t3 <- subset(ay.vcv.td3, FolSegTrans == "T")
    ay.vcv.t3 <- transform(ay.vcv.t3, Ratio = `T`/D)
    ay.vcv.t3 <- transform(ay.vcv.t3, Ratio.c = cut(Ratio, breaks = c(0,1,3,Inf), include.lowest = T))

    ddply(ay.vcv.t3, .(File, DOB, Ratio.c), summarise,
          F1 = mean(F1.n),
          N = length(F1.n))->ay.vcv.t3.mean


## @knitr analogy2
    ggplot(ay.vcv.t3.mean, aes(DOB, -F1, color = Ratio.c)) +
        geom_hline(y = 0, color = "grey80")+
        geom_point(aes(size = N)) + 
        scale_area(guide = "none") + 
        ylim(-3,1) + 
        stat_smooth(method = gam, formula = y ~ s(x), aes(weight = log1p(N)), size = 1.25) +
        scale_color_manual(values = pal[c(3,5,7)],
                           name = "Ratio t/d",
                           breaks = levels(ay.vcv.t3$Ratio.c),
                           labels = c("0 to 1x", "1 to 3x", "3+ x")
                           ) + 
                               theme_bw()


## @knitr load-Vw
    uw <- load.vowels(c("uw","Tuw"), path = path)
    ow <- load.vowels(c("owF","ow"), path = path)


## @knitr uw-cleanup
    Tuw <- subset(uw, PreSegTrans %in% c("T","D","JH",
                                         "SH","N","S",
                                         "Z","ZH","TH",
                                         "R","CH") &
                        Context %in% c("Final", "Internal"))
    uw2 <- subset(uw, (!PreSegTrans %in% c("T","D","JH",
                                         "SH","N","S",
                                         "Z","ZH","TH",
                                          "R","Y","L",
                                          "W","CH","IY0",
                                           "IY2","ER0"))&
                        Context %in% c("Final", "Internal"))
    uw2 <- subset(uw2, !(Context == "Internal" & FolSegTrans %in% c("L")))
    uw2$Context <- as.factor(as.character(uw2$Context))
    uw2$Word <- as.character(uw2$Word)
    uw2$File <- as.character(uw2$File)


## @knitr ow-cleanup
    ow2 <- subset(ow, !(Context == "Internal"& FolSegTrans=="L") & 
                        Context != "Coextensive")
    
    ow2$Context <- as.factor(as.character(ow2$Context))
    levels(ow2$Context) <- c("Final","Internal","Internal")
    
    ow2$Word <- as.character(ow2$Word)
    ow2$File <- as.character(ow2$File)


## @knitr Vw-means
    ow.means <- ddply(ow2, .(File, Sex, DOB, Context), summarise,
                      F2 = mean(F2.n), F1 = mean(F1.n), VClass = "ow")
    uw.means <- ddply(uw2, .(File, Sex, DOB), summarise,
                      F2 = mean(F2.n), F1 = mean(F1.n), VClass = "uw")
    
    aw.means$VClass <- "aw"    
    
    aw.means$File <- as.character(aw.means$File)


## @knitr Vw-mods
    
    uw.f1.mod <- gamm(F1.n ~ s(DOB, bs = "cs"), random=list(File= ~1), data = uw2)
    uw.f2.mod <- gamm(F2.n ~ s(DOB, bs = "cs"), random=list(File= ~1), data = uw2)
    
    uw.pred <- expand.grid(DOB = 1888:1991)
    uw.pred$F1 <- predict(uw.f1.mod$gam, newdata = uw.pred)
    uw.pred$F2 <- predict(uw.f2.mod$gam, newdata = uw.pred)
    
    uw.pred$VClass <- "uw"
    
    ow.f1.mods <- dlply(ow2, .(Context), function(df){
                        gamm(F1.n ~ s(DOB, bs = "cs"),random=list(File= ~1), data = df)
                    })
    ow.f2.mods <- dlply(ow2, .(Context), function(df){
                        gamm(F2.n ~ s(DOB, bs = "cs"),random=list(File= ~1), data = df)
                    })

    ow.pred <- expand.grid(DOB = 1888:1991)
    
    ow.f1.pred <- ldply(ow.f1.mods, function(x,df){
                        df$F1 <- predict(x$gam, newdata = df)
                        return(df)
                },df = ow.pred)
    ow.f2.pred <- ldply(ow.f2.mods, function(x,df){
                        df$F2 <- predict(x$gam, newdata = df)
                        return(df)
                },df = ow.pred)
    
    ow.preds <- merge(ow.f1.pred, ow.f2.pred)
    ow.preds$VClass <- "ow"


## @knitr all-Vw-preds
    aw.preds$VClass <- "aw"
    aw.preds$Phone <- as.factor(aw.preds$Nasal)
    levels(aw.preds$Phone) <- c("B","A","C")
    aw.preds$label <- aw.preds$Nasal
    
    ow.preds$Phone <- as.factor(ow.preds$Context)
    levels(ow.preds$Phone) <- c("B","A")
    ow.preds$label <- ow.preds$Context
    
    uw.pred$Phone <- "A"
    uw.pred$Phone <- as.factor(uw.pred$Phone)
        
    Vw.preds <- rbind.fill(uw.pred, ow.preds, aw.preds)


## @knitr Vw-ani
    phone.lab <- subset(Vw.preds, DOB == 1888)
    for(x in aw.2ade){
        dfnew <- subset(Vw.preds, DOB <=x)
                        
        date.df <- data.frame(x = x)
        d <- ggplot(date.df, aes(x=x, y = "Date")) + 
            geom_segment(xend=1888, x = 1991, yend=1, size = 1) + 
            xlim(1888,1999) + 
            geom_point(size = 3) + 
            geom_text(aes(label = x, y = 1.3))+
            theme_bw()+
            opts(axis.line = theme_blank(),
                 axis.text.x = theme_blank(),
                 axis.text.y=theme_blank(),
                 axis.ticks = theme_blank(),
                 axis.title.x = theme_blank(),
                 axis.title.y = theme_blank(),
                 panel.background = theme_blank(),
                 panel.border = theme_blank(),
                 panel.grid.major = theme_blank(),
                 panel.grid.minor = theme_blank())
        
        g <- ggplotGrob(d)
        
        print(ggplot(dfnew, aes(-F2,-F1, color = VClass, linetype = Phone)) + 
            geom_hline(y = 0, color = "darkgrey")+
            geom_vline(x = 0, color = "darkgrey")+
            geom_path(arrow = arrow()) + 
            geom_text(data = phone.lab, aes(x = -F2+0.1, label = label), show_guide = F)+
            theme_bw()+
            scale_linetype(guide = "none" )+
            scale_color_hue(limits = c("uw","ow","aw"))+
            ylim(-1.8, 1.3)+
            expand_limits(x = -1.52)+
            annotation_custom(grob = g, xmin=-1.6, xmax = 1.8, ymin=-2.1, ymax = -1.3))        
    }


## @knitr Vw-cor
    uw.means$Meas <- uw.means$F2
    ow.means$Meas <- ow.means$F2
    aw.means$Meas <- aw.means$Diag
    
    
    Vw.means <- rbind.fill(uw.means, 
                           subset(ow.means, Context == "Internal"),
                           subset(aw.means, Nasal == "other"))
    
    Vw.means.c <- dcast(Vw.means, File + Sex + DOB ~ VClass, value.var = "Meas")
    
    rho <- function(columns, data, indices){
        d <- data[indices,columns]
        rho <- cor(d, method = "spear", use = "complete")[1,2]
        return(rho)
    }
    
    aw.ow.boot <- boot(data = Vw.means.c,
                             columns = c("aw","ow"), 
                             statistic = rho, R = 10000)
    aw.ow.hilo <- quantile(aw.ow.boot$t,probs=c(0.025, 0.975))

    ow.uw.boot <- boot(data = Vw.means.c,
                             columns = c("ow","uw"), 
                             statistic = rho, R = 10000)
    ow.uw.hilo <- quantile(ow.uw.boot$t,probs=c(0.025, 0.975))
    
    aw.uw.boot <- boot(data = Vw.means.c,
                             columns = c("aw","uw"), 
                             statistic = rho, R = 10000)
    aw.uw.hilo <- quantile(aw.uw.boot$t,probs=c(0.025, 0.975))


## @knitr Vw-plots
  ggplot(subset(Vw.means.c), aes(aw,ow))+
      geom_point()+
      stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"))+
      theme_bw()+
      xlab("/aw/ Diagonal (F2-F1)")+
      ylab("/ow/ Backness (F2)")
    
  ggplot(subset(Vw.means.c, !is.na(uw)), aes(uw,ow))+
      geom_point()+
      stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"))+
      theme_bw()+
      xlab("/uw/ Backness (F2)")+
      ylab("/ow/ Backness (F2)")

    
  ggplot(subset(Vw.means.c, !is.na(uw)), aes(aw,uw))+
      geom_point()+
      stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"))+
      theme_bw()+
      xlab("/aw/ Diagonal (F2-F1)")+
      ylab("/uw/ backness (F2)")


## @knitr load-ey
    ey <- load.vowels(c("ey","eyF"), path = path)


## @knitr ey-setup
    eyC <- subset(ey, Context %in% c("Initial","Internal")&
                        !grepl("[AEIOU]", FolSegTrans)&
                        FolSegTrans != "L")
    eyC$File <- as.factor(as.character(eyC$File))
    eyC$Word <- as.factor(as.character(eyC$Word))
    
    eyF <- subset(ey, Context == "Final")
    eyF$File <- as.factor(as.character(eyF$File))
    eyF$Word <- as.factor(as.character(eyF$Word))


## @knitr ey-mods
    eyC.f1.mod <- gamm(F1.n ~ s(DOB, bs = "cs"), random = list(File = ~1), data = eyC)
    eyC.f2.mod <- gamm(F2.n ~ s(DOB, bs = "cs"), random = list(File = ~1), data = eyC)
    
    eyF.f1.mod <- gamm(F1.n ~ s(DOB, bs = "cs"), random = list(File = ~1), data = eyF)
    eyF.f2.mod <- gamm(F2.n ~ s(DOB, bs = "cs"), random = list(File = ~1), data = eyF)
    
    eyC.pred <- data.frame(DOB = 1888:1991, VClass = "ey", Context = "C")
    eyF.pred <- data.frame(DOB = 1888:1991, VClass = "ey", Context = "F")
    
    eyC.pred$F1 <- predict(eyC.f1.mod$gam, newdata = eyC.pred)
    eyC.pred$F2 <- predict(eyC.f2.mod$gam, newdata = eyC.pred)
    
    eyF.pred$F1 <- predict(eyF.f1.mod$gam, newdata = eyF.pred)
    eyF.pred$F2 <- predict(eyF.f2.mod$gam, newdata = eyF.pred)
    
    eys.pred <- rbind.fill(eyC.pred, eyF.pred)


## @knitr ani-setup
    ay.preds$VClass <- "ay"
    ay.preds$VClass <- as.factor(ay.preds$VClass)
    ay.preds$Phone <- as.factor(ay.preds$Voice)
    levels(ay.preds$Phone) <- c("B","A")
    
    eys.pred$Phone <- as.factor(eys.pred$Context)
    levels(eys.pred$Phone) <- c("A","B")
    
    Vy.preds <- rbind.fill(ay.preds, eys.pred)


## @knitr Vy-ani
    vy.2ade <- sort(unique(floor(Vy.preds$DOB/2)*2))[-1]
    for(x in vy.2ade){
        dfnew <- subset(Vy.preds, DOB <=x)
                        
        date.df <- data.frame(x = x)
        d <- ggplot(date.df, aes(x=x, y = "Date")) + 
            geom_segment(xend=1888, x = 1991, yend=1, size = 1) + 
            xlim(1888,1999) + 
            geom_point(size = 3) + 
            geom_text(aes(label = x, y = 1.3))+
            theme_bw()+
            opts(axis.line = theme_blank(),
                 axis.text.x = theme_blank(),
                 axis.text.y=theme_blank(),
                 axis.ticks = theme_blank(),
                 axis.title.x = theme_blank(),
                 axis.title.y = theme_blank(),
                 panel.background = theme_blank(),
                 panel.border = theme_blank(),
                 panel.grid.major = theme_blank(),
                 panel.grid.minor = theme_blank())
        
        g <- ggplotGrob(d)
        
        print(ggplot(dfnew, aes(-F2,-F1, color = VClass, linetype = Phone))+
            geom_hline(y = 0, color = "darkgrey")+
            geom_vline(x = 0, color = "darkgrey")+
            geom_path(arrow = arrow())+
            ylim(-1.8, 1.3)+
            xlim(-1.52, 1.5)+
            scale_linetype(limits = c("A","B"), guide = "none")+
            scale_color_brewer(limits = c("ey","ay"),type="qual",palette=2)+
            theme_bw()+
            annotation_custom(grob = g, xmin=-1.6, xmax = 1.8, ymin=-2.1, ymax = -1.3))        
    }    

    
    



## @knitr Vy-cor-setup
    eyC.mean <- ddply(eyC, .(File, DOB, Sex, VClass), summarise,
                      F1 = mean(F1.n), F2 = mean(F2.n), Diag = mean(F2.n-F1.n),
                      N = length(F1.n))
    eyC.mean$VClass <- "ey"
    eyC.mean$Meas <- eyC.mean$Diag
    
    ay0.mean <- subset(ay.comp.mean, Voice == "voiceless")
    ay0.mean$VClass <- "ay"
    ay0.mean$Meas <- -ay0.mean$F1
    
    Vy.mean <- rbind.fill(ay0.mean, eyC.mean)
    Vy.mean.c <- dcast(Vy.mean, File + DOB + Sex ~ VClass, value.var = "Meas")


## @knitr Vy-cor
    rho <- function(columns, data, indices){
        d <- data[indices,columns]
        rho <- cor(d, method = "spear", use = "complete")[1,2]
        return(rho)
    }
    
    ay.ey.boot <- boot(data = Vy.mean.c,
                             columns = c("ay","ey"), 
                             statistic = rho, R = 10000)
    ay.ey.hilo <- quantile(ay.ey.boot$t,probs=c(0.025, 0.975))


## @knitr ay-ey-plot
    ggplot(Vy.mean.c, aes(ay, ey)) + 
        geom_point()+
        stat_smooth(method = gam, formula = y ~ s(x, bs = "cs"))+
        theme_bw()+
        xlab("Pre-voicless /ay/ height (-F1)")+
        ylab("/eyC/ Raising (F2-F1)")


## @knitr ay-plot-again



