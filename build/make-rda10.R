## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM10 Prioritization of Information in WM #####################
pth  <- "BenchmarksWM.Data/BM10.1.RetroCue/"


Retrocue1 <- read.table(paste0(pth,"SouzaRetroCueVisVerb.dat"), header=F)
names(Retrocue1) <- c("id", "session", "trial", "visverb", "cuecond", "setsize", "color1", "color2", "color3", "color4", "color5", "color6", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "cuedloc", "ptype", "probecolor", "rt", "corr")
# visverb: 1 = visual, 2 = verbal
# cuecond: 1 = RC(100 ms), 2 = RC(400ms), 3 = RC(2000ms), 4 = no cue
souza14a <- Retrocue1[which(Retrocue1$visverb == 2),]


Retrocue2 <- read.table(paste0(pth,"SouzaRetroCueVis.dat"), header=F)
names(Retrocue2) <- c("id", "session", "trial", "cuecond", "setsize", "color1", "color2", "color3", "color4", "color5", "color6", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "cuedloc", "ptype", "probecolor", "rt", "corr")
# cuecond: 1 = RC(100 ms), 2 = RC(400ms), 3 = RC(1000ms), 4 = RC(2000 ms) 5 = no cue
# loc = location of stimulus (angle)
# ptype: 1 = match, 2 = intrusion, 3 = new probe

check1 <- Retrocue1[which(Retrocue1$visverb == 1),]

souza1b <- Retrocue2
souza1b$probetype[souza1b$ptype == 1] <- "match"
souza1b$probetype[souza1b$ptype == 2] <- "intrusion"
souza1b$probetype[souza1b$ptype == 3] <- "new probe"

souza1b$PCI[souza1b$cuecond == 1] <- 100
souza1b$PCI[souza1b$cuecond == 2] <- 400
souza1b$PCI[souza1b$cuecond == 3] <- 1000
souza1b$PCI[souza1b$cuecond == 4] <- 2000
souza1b$PCI[souza1b$cuecond == 5] <- 0

souza1b$condition[souza1b$cuecond == 1] <- "100 ms"
souza1b$condition[souza1b$cuecond == 2] <- "400 ms"
souza1b$condition[souza1b$cuecond == 3] <- "1000 ms"
souza1b$condition[souza1b$cuecond == 4] <- "2000 ms"
souza1b$condition[souza1b$cuecond == 5] <- "no cue"

souza1b$trial <- (souza1b$cuecond-1)*128 + souza1b$trial
souza1b$trial <- (souza1b$session-1)*640 + souza1b$trial

souza14 <- souza1b %>% select(id, session, trial, condition, PCI, setsize, probetype, 
                              corr, rt)

souza14 <- souza14 %>% rename(subj = id, acc = corr)
save(souza14, file = "./pkg/data/souza14.rda")

### plotting

pd <- aggregate(acc ~ condition + setsize, data = souza14, FUN = mean)
plot(c(1,6), c(0.7,1.0), type = "n", xlab = "Set Size",
     ylab = "Proportion correct", main = "Retro-Cue Effect in Change Detection", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = c(1,2,3,4,5,6), 
     cex.axis = 0.7)
lines(x = pd$setsize[pd$condition == "no cue"], 
      y = pd$acc[pd$condition == "no cue"], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$setsize[pd$condition == "100 ms"], 
      y = pd$acc[pd$condition == "100 ms"], 
      type = "b", lty = 2, pch = 16, col = "lightgrey")
lines(x = pd$setsize[pd$condition == "400 ms"], 
      y = pd$acc[pd$condition == "400 ms"], 
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$setsize[pd$condition == "1000 ms"], 
      y = pd$acc[pd$condition == "1000 ms"], 
      type = "b", lty = 4, pch = 18, col = "darkgrey")
lines(x = pd$setsize[pd$condition == "2000 ms"], 
      y = pd$acc[pd$condition == "2000 ms"], 
      type = "b", lty = 5, pch = 19, col = "black")
legend(1, 0.7, c("no cue", "100 ms",  "400 ms", "1000 ms", "2000 ms"), lty = c(1:4,5), 
       pch=15:19, col = c("black", "lightgrey", "grey", "darkgrey", "black"),
       horiz = F, cex = 0.6, yjust = 0, xjust = 0, title = "Post-cue interval:")




###### Oberauer & Lin (2017) Retro-cue in continuous reproduction

Retrocue <- read.table(paste0(pth, "colorwheel5.dat"), header=F)
names(Retrocue) <- c("id", "session", "cue", "xxx", "trial", "setsize", "cuecond", "cuelocation", 
                     "stim1", "loc1", "stim2", "loc2", "stim3", "loc3", "stim4", "loc4", "stim5", "loc5", "stim6", "loc6", "stim7", "loc7", "stim8", "loc8", "response")

# cue: 1 = informative cue, 3 = neutral cue
# cuecond: -1 = neutral, 1 = valid, 0 = invalid
# stim1 to stim8: colors on the color wheel (in degrees), stim1 is the target
# loc1 to loc8: locations of the colors on the virtual circle (1 to 13), loc1 is the target location
# Paul Bays' wrap function -> signed angular difference (radians)!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}

Retrocue$targetrad <- pi*Retrocue$stim1/180
Retrocue$responserad <- pi*Retrocue$response/180
Retrocue$diffrad <- wrap(Retrocue$responserad - Retrocue$targetrad)
Retrocue$errorrad <- abs(Retrocue$diffrad)
Retrocue$errordeg <- 180*Retrocue$errorrad/pi

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
ptypes <- c(21:25, 21:25)
aggdat <- aggregate(errordeg ~ id + setsize + cuecond, data=Retrocue, FUN=mean)

lineplot.ci3(data=aggdat, dv="errordeg", iv=c("setsize", "cuecond"), id=1, x=1, off=0.05, ylim=c(0,80),
             cex=1.3, lty=1, pt = ptypes[1:3], ptcol=colors[1:3], xlab="Set Size", ylab="Error (deg)")
legend(4,0,c("Neutral", "Invalid", "Valid"), pt.bg=colors[1:3], lty=1, pch=ptypes[1:3], pt.cex = 1.5, yjust=0)

# trial

Retrocue$trial <- (Retrocue$session-1)*300 + Retrocue$trial

Retrocue$condition[Retrocue$cuecond == -1] <- "neutral"
Retrocue$condition[Retrocue$cuecond == 1] <- "valid"
Retrocue$condition[Retrocue$cuecond == 0] <- "invalid"


oberauer17 <- Retrocue %>% select(id, session, trial, condition, setsize, targetrad,
                                  responserad, errorrad, errordeg)
oberauer17 <- oberauer17 %>% rename(subj = id, target = targetrad, response = responserad,
                                    error = errorrad)

save(oberauer17, file = "./pkg/data/oberauer17.rda")

# Plot:

pd <- aggregate(errordeg ~ + setsize + condition, data=oberauer17, FUN=mean)
plot(c(1,8), c(0,80), type = "n", xlab = "Set Size",
     ylab = "Error (deg)", main = "Retro-Cue Effect in Continuous Reproduction", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = c(2,4,6,8), 
     cex.axis = 0.7)
lines(x = pd$setsize[pd$condition == "neutral"], 
      y = pd$errordeg[pd$condition == "neutral"], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$setsize[pd$condition == "valid"], 
      y = pd$errordeg[pd$condition == "valid"], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$setsize[pd$condition == "invalid"], 
      y = pd$errordeg[pd$condition == "invalid"], 
      type = "b", lty = 3, pch = 17)
legend(8, 0, c("neutral", "valid", "invalid"), lty = 1:3, 
       pch=15:17, 
       horiz = F, cex = 0.6, yjust = 0, xjust = 1, title = "Cue condition:")



#### 10.2

rm(list=ls())
############### BM10 Prioritization of Information in WM #####################
pth  <- "BenchmarksWM.Data/BM10.2.ItemSwitchEffects/"


data<-read.csv(paste0(pth,'Hedge&Leonardsdata.csv'))
# remove trials not included in RT analysis
data2<-data[data$Block!=1 & data$TrialCorrect!=0 & data$Trial_type!=0,] 
#aggregate
aggdata <-aggregate(data2$RT, by=list(data2$Experiment,data2$Participant,data2$Trial_type), 
                    FUN=median, na.rm=TRUE)
names(aggdata) <- c("Experiment", "PPT","Update_type","RT")
aggdata$PPT<-as.factor(aggdata$PPT)
aggdata$Update_type<-as.factor(aggdata$Update_type)
aggdata$Experiment<-as.factor(aggdata$Experiment)


pd <- aggregate(data2$RT, by=list(data2$Experiment,data2$Trial_type), 
                FUN=median, na.rm=TRUE)

pdd <- aggregate(RT ~ Trial_type+Experiment, data = data2, FUN = median, na.rm = T)

barplot(height = pdd$RT)

hedge13 <- data %>% rename(exp = Experiment, subj = Participant, block = Block,
                           trial = Trial_sequence, length = Sequence_length, 
                           serpos = Update, update_type = Trial_type,
                           rt = RT, acc = TrialCorrect)
hedge13 <- hedge13 %>% select(exp,subj,block,trial,length,serpos,update_type,acc,rt)

hedge13$update_type[hedge13$update_type == 0] <- "first update"
hedge13$update_type[hedge13$update_type == 1] <- "repeat"
hedge13$update_type[hedge13$update_type == 2] <- "switch"

hedge13$update_type <- as.factor(hedge13$update_type)

save(hedge13, file = "./pkg/data/hedge13.rda") 

### Simple barplot
hedge13included <- hedge13[which(hedge13$block != 1 & hedge13$update_type != "first update"
                                 & hedge13$acc == 1),]
pd <- aggregate(rt ~ update_type+exp, data = hedge13included, FUN = median, na.rm=T)


bp = barplot(pd$rt, space = c(0.2,0.2,0.7,0.2), col=c("lightgrey", "darkgrey", 
                                                      "lightgrey", "darkgrey"),
             ylab="Median RT", xlab="Update Type", axisnames=F, ylim = c(0,1.5))
text (1.3,1.3,"Experiment 1",cex=0.8)
text (4.2,1.3,"Experiment 2",cex=0.8)
box()
axis(1, at = bp[,1], labels=c("repeat", "switch", "repeat", "switch"), cex.axis=0.7)


### Oberauer (2006)
rm(list=ls())
############### BM10 Prioritization of Information in WM #####################
pth  <- "BenchmarksWM.Data/BM10.2.ItemSwitchEffects/"

data <- read.table(paste0(pth,"NBACK.ROH"))
names(data) <- c('subj', 'session', 'trial','setsize', paste0('init.dig', 1:5),
                 paste0('pres.dig', 1:20), paste0(c('x','y'), rep(1:20, each = 2)), 
                 paste0('update_type', 1:20), paste0('dist', 1:20), paste0('ptype', 1:20),
                 paste0('intrusion', 1:20), paste0('lag', 1:20), paste0('rt',1:20),
                 paste0('acc', 1:20))

### okay. 20 digits presented per trial, regardless of setsize
### thus, everything except the init.digs can be gathered into long format, in principle.

d_dig <- gather(data, key = "serpos", value = "digit", 10:29)
d_x <- gather(data, key = "serpos", value = "column", seq(30, 69, by = 2))
d_y <- gather(data, key = "serpos", value = "row", seq(31,69,by=2))
d_type <- gather(data, key = "serpos", value = "update_type", 70:89)
d_dist <- gather(data, key = "serpos", value = "dist", 90:109)
d_probe <- gather(data, key = "serpos", value = "ptype", 110:129)
d_intrusion <- gather(data, key = "serpos", value = "intrusion", 130:149)
d_lag <- gather(data, key = "serpos", value = "lag", 150:169)
d_rt <- gather(data, key = "serpos", value = "rt", 170:189)
d_acc <- gather(data, key = "serpos", value = "acc", 190:209)


d_comp <- d_dig %>% select(subj, session, trial, setsize, init.dig1, init.dig2,
                           init.dig3, init.dig4, init.dig5, serpos, digit)
ob06 <- cbind(d_comp, d_x$column, d_y$row, d_type$update_type, d_dist$dist,
              d_probe$ptype, d_intrusion$intrusion, d_lag$lag, d_rt$rt, d_acc$acc)

ob06 <- ob06 %>% rename(column = `d_x$column`, row = `d_y$row`, 
                        update_type = `d_type$update_type`, dist = `d_dist$dist`,
                        ptype = `d_probe$ptype`, intrusion = `d_intrusion$intrusion`,
                        lag = `d_lag$lag`, rt = `d_rt$rt`, acc = `d_acc$acc`)

### now, a good thing would be to check the switches

ob06$serpos <- as.numeric(substr(ob06$serpos,9,10))

### plot

pd <- aggregate(rt ~ setsize+update_type, data = ob06[which(ob06$session == 10 & 
                                                              ob06$update_type != -1),],
                FUN = mean)
plot(c(1,5), c(0,1800), type = "n", xlab = "Set Size",
     ylab = "RT (ms)", main = "Switch Costs in Session 10", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5), labels = c(1,2,3,4,5), 
     cex.axis = 0.7)
lines(x = pd$setsize[pd$update_type == 1], 
      y = pd$rt[pd$update_type == 1], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$setsize[pd$update_type == 0], 
      y = pd$rt[pd$update_type == 0], 
      type = "b", lty = 2, pch = 16)

legend(5, 1800, c("no-switch", "switch"), lty = 1:2, 
       pch=15:16, 
       horiz = F, cex = 0.9, yjust = 1, xjust = 1)

ob06$rt[ob06$rt > 30000] <- NA


### rename some variable values for clarity:

ob06$update_type[ob06$update_type == -1] <- "first item"
ob06$update_type[ob06$update_type == 0] <- "no-switch"
ob06$update_type[ob06$update_type == 1] <- "switch"

ob06$ptype[ob06$intrusion == 1] <- "intrusion"
ob06$ptype[ob06$ptype == 2] <- "mismatch"
ob06$ptype[ob06$ptype == 1] <- "match"

oberauer06 <- ob06 %>% select(subj, session, trial, setsize, init.dig1, init.dig2,
                              init.dig3, init.dig4, init.dig5, serpos, digit, column,
                              row, update_type, dist, ptype, lag, acc, rt)
save(oberauer06, file = "./pkg/data/oberauer06.rda")

### Approximate replication of Figure 3 (top panel) in Oberauer (2006)
pd <- aggregate(rt ~ setsize+update_type+session, 
                data = oberauer06[which(oberauer06$update_type != "first item"),],
                FUN = mean)
bgk <- c("white","black")
plot(0,0, xlim=c(0,50),ylim=c(0,1800), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Set size (Sessions 1-10)", ylab="RT (ms)",cex.lab=1.0,cex.axis=1.0)
title("Switch Costs across Sessions")
axis(1, at=c(1:50), labels = rep(1:5,10), lwd=0.5, lwd.ticks=0.2,cex.axis=0.5)
axis(2, at=seq(from=0,to=1800,by=200),lwd=1, lwd.ticks=1,cex.axis=1.0)
for (k in c(1:10)) {
  tbp <- pd[which(pd$session == k),]
  lines(c(((k-1)*5+1):((k-1)*5+5)),tbp$rt[1:5],lwd=1,lty=1)
  lines(c(((k-1)*5+2):((k-1)*5+5)),tbp$rt[6:9],lwd=1,lty=2)
  points(c(((k-1)*5+1):((k-1)*5+5)),tbp$rt[1:5],pch=22,bg=bgk[1],cex=0.5)
  points(c(((k-1)*5+2):((k-1)*5+5)),tbp$rt[6:9],pch=19,bg=bgk[2],cex=0.5)
}

legend(50,1800,c("no-switch", "switch"),lty=c(1:2),pch=c(22,19),pt.bg=bgk,cex=0.6,pt.cex=1.0, yjust = 1, xjust = 1)


# cuecond: 1 = RC(100 ms), 2 = RC(400ms), 3 = RC(2000ms), 4 = no cue

souza14a <- souza14a %>% rename(subj = id, acc = corr)

souza14a$probetype[souza14a$ptype == 1] <- "match"
souza14a$probetype[souza14a$ptype == 2] <- "intrusion"
souza14a$probetype[souza14a$ptype == 3] <- "new probe"

souza14a$CTI[souza14a$cuecond == 1] <- 100
souza14a$CTI[souza14a$cuecond == 2] <- 400
souza14a$CTI[souza14a$cuecond == 3] <- 2000
souza14a$CTI[souza14a$cuecond == 4] <- 0

souza14a$condition[souza14a$cuecond == 1] <- "100 ms"
souza14a$condition[souza14a$cuecond == 2] <- "400 ms"
souza14a$condition[souza14a$cuecond == 3] <- "2000 ms"
souza14a$condition[souza14a$cuecond == 4] <- "no cue"

souza14a <- souza14a %>% select(subj, session, trial, condition, CTI, setsize, probetype, 
                              acc, rt)

save(souza14a, file = "./pkg/data/souza14a.rda")

souza14 <- souza14 %>% rename(CTI = PCI)

save(souza14, file = "./pkg/data/souza14.rda")

### Reproduce part of Figure 2 from Shepherdson et al. (2018)
### without confidence interval

data("souza14")
plotd <- souza14a[which(!(souza14a$rt < 0.2) & !(souza14a$rt > 5.0)),]
plotd <- aggregate(rt ~ setsize*CTI*subj, data=plotd, FUN=mean)
plot <- interaction.plot(plotd$setsize, plotd$CTI, plotd$rt, type = "b",
                         pch= 15:19, xlab = "Set size", ylab = "Mean RT",
                         legend = F, xtick = T, ylim = c(0.4,1.3),
                         main = "Set size effect on RT with retro cues")
legend(1, 1.2, c("No Cue", "100 ms", "2000 ms", "400 ms"), pch=15:19, yjust=1)
### watch out: legend is messy

check <- shepherdson18[which(shepherdson18$exp == "Exp 1"), ]

check2 <- souza14[which(!(souza14$rt < 0.2) & !(souza14$rt > 5.0)),]

mean(check2$rt)
