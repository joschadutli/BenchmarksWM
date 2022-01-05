## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM1.2 #####################
pth  <- "BenchmarksWM.Data/BM1.2.SetsizeRT/"

## Issues w/ original R script:
## - Duplicate data sets (.xls and .txt).
## - BM1.1.SetsizeAccuracy.R:
##     - requires a directory "functions" which is absent
##     - assumes case-insentive filenames 

### Donkin & Nosofsky 2012b ###

### here should be a sumary of changes

### load data:
fnam <- paste0(pth, "Donkin.Sternberg/subj")
SternFast <- NULL
SternSlow <- NULL
for (subj in 1:7) {
  filename = paste(fnam, as.character(subj), ".txt", sep="")
  d <- read.table(filename, header=T)  
  names(d)[1:2] <- c("x1", "id")
  d$id <- subj
  d <- d[,2:11]
  if (subj < 5) SternFast <- rbind(SternFast, d) #Chris: subj 1-4 did the fast version
  if (subj > 4) SternSlow <- rbind(SternSlow, d) #Chris: subj 5-7 did the slow version
}
SternFastAgg <- aggregate(RT ~ id + SetSize + Lag, data=SternFast[SternFast$use==1,], FUN=mean)
SternSlowAgg <- aggregate(RT ~ id + SetSize + Lag, data=SternSlow[SternSlow$use==1,], FUN=mean)

SternFastAgg$rate <- 'fast'
SternSlowAgg$rate <- 'slow'

don <- rbind(SternSlowAgg, SternFastAgg)
don <- don %>% rename(pos = Lag) # substitute any pos > size with "new" (or "lure")
don <- don %>% rename(size = SetSize) %>% rename(rt = RT) %>% rename(subject = id)
don$pos[don$pos > don$size] <- 6
don <- don %>% arrange(subject, size, pos, rate)
don <- don %>% select(subject, rate, size, pos, rt)

donkin12b <- don
save(donkin12b, file="pkg/data/donkin12b.rda")
rm(don)

### Reproduce figure 3 in Oberauer et al. (2018)

data("donkin12b")
dplot <- aggregate(rt ~ pos*size*rate, data = donkin12b, FUN = mean)
dplot$gv <- dplot$size
dplot$gv[dplot$pos == 6] <- 6

dfast <- dplot[which(dplot$rate == "fast"),]
dslow <- dplot[which(dplot$rate == "slow"),]

ggplot(data=dfast, mapping = aes(y=rt, x=as.factor(pos), group=interaction(size, gv), shape=as.factor(size))) +
  ggtitle("Fast presentation rate") +
  geom_point() + geom_line() + ylim(.35,.7) + ylab("RT (s)") + 
  scale_x_discrete(name = "Serial Position",
                     labels=c("1","2","3","4","5","new"),
                     breaks=c(1,2,3,4,5,6)) +
  scale_shape_discrete(name="Set size")

ggplot(data=dslow, mapping = aes(y=rt, x=as.factor(pos), group=interaction(size, gv), shape=as.factor(size))) +
  ggtitle("Slow presentation rate") +
  geom_point() + geom_line() + ylim(.35,.7) + ylab("RT (s)") + 
  scale_x_discrete(name = "Serial Position",
                   labels=c("1","2","3","4","5","new"),
                   breaks=c(1,2,3,4,5,6)) +
  scale_shape_discrete(name="Set size")

par(mfrow=c(1,2))
dfast$pos[dfast$pos == 6] <- "new"
interaction.plot(dfast$pos, dfast$size, dfast$rt, type = "b", pch = 15:19, 
                 ylim = c(.35,.70), xlab = "Serial Position", 
                 ylab = "Mean RT", legend = F, xtick=T,
                 main = "Fast presentation rate")
dslow$pos[dslow$pos == 6] <- "new"
interaction.plot(dslow$pos, dslow$size, dslow$rt, type = "b", pch = 15:19,
                 ylim = c(.35,.70), xlab = "Serial Position",
                 ylab = "Mean RT", trace.label = "Set size", 
                 main = "Slow presentation rate", xtick = T)
  
### Gilchrist & Cowan
### summary of changes

### load data:

fnam1 <- paste0(pth, "Gilchrist.Cowan/renamed.data/exp1.xlsx")
fnam2 <- paste0(pth, "Gilchrist.Cowan/renamed.data/exp2.xlsx")
exp1 <- read_excel(fnam1, sheet = "Raw Data_Experiment1")
exp2 <- read_excel(fnam2, sheet = "Raw Data_Experiment 2")

### Experiment 1

exp1 <- exp1 %>% dplyr::rename(subject = Subject) %>% dplyr::rename(order = Order) %>% dplyr::rename(size = SetSize) %>%
  dplyr::rename(change = `Change?`) %>% dplyr::rename(acc = Accuracy) %>%
  dplyr::rename(rt = RT) %>% dplyr::rename(location = `Central or Location Probe?`)
exp1$rt[exp1$`Outlier?` == 'yes'] <- NA
exp1$change[exp1$change == 'Yes'] <- 1
exp1$change[exp1$change == 'No'] <- 0

exp1 <- exp1 %>% select(subject, order, location, size, change, acc, rt)
exp1$exp <- 1

### Experiment 2
exp2 <- exp2 %>% dplyr::rename(subject = Subject) %>% dplyr::rename(order = Order) %>% dplyr::rename(size = `Set Size`) %>%
  dplyr::rename(change = `Change?`) %>% dplyr::rename(acc = Accuracy) %>%
  dplyr::rename(rt = RT) %>% dplyr::rename(location = `Central or Location Probe?`)
exp2$rt[exp2$`Outlier?` == 'yes'] <- NA
exp2$change[exp2$change == 'Yes'] <- 1
exp2$change[exp2$change == 'No'] <- 0


exp2 <- exp2 %>% select(subject, order, location, size, change, acc, rt)
exp2$exp <- 2

wrong.pt2 <- aggregate(acc ~ subject*order*change*size, data = exp1, FUN=sum)
wrong.pt2$subject[wrong.pt2$acc<17]
## cant find the to-be-excluded dataset because bluck number has not been recorded
wrong.pt3 <- aggregate(acc ~ subject*order*change*size*location, data = exp2, FUN=sum)
wrong.pt3$subject[wrong.pt3$acc<4] # these are the to-be-excluded participants
#8,12,2,6,11 according to the criteria in Gilchrist & Cowan (2014)
#However, they only excluded 3 (presumably they included subject 11, who's data is almost complete (except for last set size..))
exp2 <- exp2[ which(!(exp2$subject %in% c(8,11,12,26))), ]


## put into one data set:
gilch <- rbind(exp1, exp2)
gilch <- gilch %>% select(exp, subject, order, location, size, change, acc, rt)
gilchrist14 <- gilch
save(gilchrist14, file="pkg/data/gilchrist14.rda")

### for help file
### Reproduce plot of Figure 4 in Gilchrist & Cowan (2014).

exp2 <- gilchrist14[which(gilchrist14$exp == 2),]
exp2$plotv[exp2$location == "Central" & exp2$change == 0] <- "Central-Old"
exp2$plotv[exp2$location == "Central" & exp2$change == 1] <- "Central-New"
exp2$plotv[exp2$location == "Location" & exp2$change == 0] <- "Location-Old"
exp2$plotv[exp2$location == "Location" & exp2$change == 1] <- "Location-New"

exp2 <- exp2[which(exp2$acc == 1),]

plotd <- aggregate(rt ~size*plotv, data = exp2, FUN = mean, na.rm=TRUE)
plotd %>% ggplot(aes(x=size, y=rt, color=plotv)) +
  geom_point() + geom_line() + 
  scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
  ylim(600,1100) +
  xlab("Set size") +
  ylab("Mean RT (ms)") +
  theme(legend.title = element_blank())

### Shepherdson et al. (2018) data on RTs 
## Read raw data of Experiment S1b (= Exp. 1b in Souza et al. (2014))
filename = "./Shepherdson.RetroCue/Souza1b(L200H5000).csv"
d1 <- read.csv(filename, header=T)  


## Read raw data of Experiment S1a (visual data = Exp. 1a in Souza et al. (2014))
filename = "./Shepherdson.RetroCue/VisualVerbal(L200H5000).csv"
d2 <- read.csv(filename, header=T)  

## Read raw data of Experiment 2
filename = "./Shepherdson.RetroCue/word_data(L200H5000).csv"
d3 <- read.csv(filename, header=T)  

d1$exp <- "Exp S1b"
d1 <- d1 %>% rename(subj = subj_idx) %>% rename(CTI = CSI)
d1$task <- "vis"
d1 <- d1 %>% select(exp, subj, task, size, probe, CTI, correct, rt)


d2$exp <- 0
d2$exp[d2$task == "vis"] <- "Exp S1a"
d2$exp[d2$task == "ver"] <- "Exp 1"
d2 <- d2 %>% rename(subj = subj_idx) %>% rename(CTI = CSI)
d2$CTI <- as.character(d2$CTI)
d2$CTI[d2$CTI == "0"] <- "No"
d2$CTI <- as.factor(d2$CTI)
d2 <- d2 %>% select(exp, subj, task, size, probe, CTI, correct, rt)

d3$exp <- "Exp 2"
d3 <- d3 %>% rename(subj = subj_idx) %>% rename(CTI = CSI)
d3$task <- "ver"
d3 <- d3 %>% select(exp, subj, task, size, probe, CTI, correct, rt)

shepherdson18 <- rbind(d1, d2, d3)
shepherdson18$subj[shepherdson18$exp == "Exp S1b"] <- shepherdson18$subj[shepherdson18$exp == "Exp S1b"] +100
shepherdson18$subj[shepherdson18$exp == "Exp 1"] <- shepherdson18$subj[shepherdson18$exp == "Exp 1"] + 200
shepherdson18$subj[shepherdson18$exp == "Exp 2"] <- shepherdson18$subj[shepherdson18$exp == "Exp 2"] + 300
save(shepherdson18, file="./pkg/data/shepherdson18.rda")


### plot

data("shepherdson18")
plotd <- shepherdson18[which(shepherdson18$exp == "Exp 1"), ]
plotd <- aggregate(rt ~ size*CTI, data=plotd, FUN=mean)
plot <- interaction.plot(plotd$size, plotd$CTI, plotd$rt, type = "b",
                         pch= 15:19, xlab = "Set size", ylab = "Mean RT",
                         legend = F, xtick = T, ylim = c(0.4,1.3),
                         main = "Set size effect on RT with retro cues")
legend(1, 0.8, c("No Cue", "100 ms", "2000 ms", "400 ms"), pch=15:19, yjust=0)

library(tidyr)
grandmean <- mean(plotd$rt)
wided <- spread(data=plotd, key=size, value=rt)
wided$casemean <- rowMeans(subset(wided, select= c(`1`, `2`, `4`, `6`)), na.rm = T)
wided$adj <- grandmean - wided$casemean
wided$`1` <- wided$`1`+wided$adj
wided$`2` <- wided$`2`+wided$adj
wided$`4` <- wided$`4`+wided$adj
wided$`6` <- wided$`6`+wided$adj
d <- gather(data=wided, key = "size", value = "rt", 3:6)
d_se <- aggregate(rt ~ size*CTI, data=d, FUN = sd)
d_m <- aggregate(rt ~ size*CTI, data=d, FUN = mean)
d_se <- d_se %>% rename(sd = rt)
d_m <- cbind(d_m, d_se$sd)
d_m$ci <- d_m$rt + 1.96*(d_m$`d_se$sd`/sqrt(16))


### Now read in data from Towse et al. (2008)

# Load data for simple and complex span
fnam <- paste0(pth, "Towse.ComplexSpan/Experimental Psychology E2 recall timing data.xlsx")
timedatLL2 = as.data.frame(read_excel(fnam, sheet="LL2", range="A1:O85"))  
fnam <- paste0(pth, "Towse.ComplexSpan/Experimental Psychology E2 recall timing data.xlsx")
timedatLL3 = as.data.frame(read_excel(fnam, sheet="LL3", range="A1:R85")) 
fnam <- paste0(pth, "Towse.ComplexSpan/Experimental Psychology computer records.xlsx")
recalldat = as.data.frame(read_excel(fnam, sheet="Collated", range="A3:Q87"))

### assign individual subject id's
### listlength = 2
timedatLL2$subj <- 0
for (i in 1:14) {
  timedatLL2$subj[grepl(i,timedatLL2$Participant, fixed = TRUE)] <- i
}
for (j in 1:84) {
  if (grepl("BSEP", timedatLL2$Participant[j], fixed=T)) {
    timedatLL2$subj[j] <- timedatLL2$subj[j]+100
  } else {
    timedatLL2$subj[j] <- timedatLL2$subj[j]
  }
}
timedatLL2$listlength <- 2

### listlength = 3
timedatLL3$subj <- 0
for (i in 1:14) {
  timedatLL3$subj[grepl(i,timedatLL3$Participant, fixed = TRUE)] <- i
}
for (j in 1:84) {
  if (grepl("BSEP", timedatLL3$Participant[j], fixed=T)) {
    timedatLL3$subj[j] <- timedatLL3$subj[j]+100
  } else {
    timedatLL3$subj[j] <- timedatLL3$subj[j]
  }
}
timedatLL3$listlength <- 3

### compute rowMeans for the two listlengths from recalldat
LL2 <- subset(recalldat, select = SP1...4:SP2...5)
LL2$acc <- rowMeans(LL2)

LL3 <- subset(recalldat, select = SP1...6:SP3...8)
LL3$acc <- rowMeans(LL3)

### attach acc to timedat datasets

timedatLL2 <- cbind(timedatLL2, acc = LL2$acc)

timedatLL3 <- cbind(timedatLL3, acc = LL3$acc)

### compute meaningful means
### -> set NA's in col's 9:14
timedatLL2$`Mean Gap` <- timedatLL2$`Gap 1-2`
timedatLL2 <- lapply(timedatLL2, function(x) replace(x, x=="-", NA))
dat2 <- as.data.frame(timedatLL2)

dat2[9:14] <- lapply(dat2[9:14], function(y) as.numeric(as.character(y)))

dat2 <- dat2 %>% rename(prep.int = Cue.Recall) %>% rename(recall.word = Mean.Word) %>% rename(pause = Mean.Gap) %>% rename(response = Response) %>% rename(condition = Condition) %>% rename(set = Set) %>% rename(trial = Trial)

dat2$read.time <- rowMeans(subset(dat2, select = Sentence.1:Sentence.2))

d2 <- dat2 %>% select(subj, set, condition, trial, listlength, read.time, prep.int, recall.word, pause, response, acc)

### do the same for listlength 3
timedatLL3 <- lapply(timedatLL3, function(x) replace(x, x=="-", NA))
dat3 <- as.data.frame(timedatLL3)

dat3[10:17] <- lapply(dat3[10:17], function(y) as.numeric(as.character(y)))

dat3 <- dat3 %>% rename(prep.int = Cue.Recall) %>% rename(recall.word = Mean.Word) %>% rename(pause = Mean.Gap) %>% rename(response = Response) %>% rename(condition = Condition) %>% rename(set = Set) %>% rename(trial = Trial)

dat3$read.time <- rowMeans(subset(dat3, select = Sentence.1:Sentence.3))
dat3$response[dat3$response == "Check"] <- "Incorrect"

d3 <- dat3 %>% select(subj, set, condition, trial, listlength, read.time, prep.int, recall.word, pause, response, acc)

towse08 <- rbind(d2,d3)
save(towse08, file="./pkg/data/towse08.rda")

data("towse08")
plotp <- aggregate(prep.int ~ condition*listlength, data = towse08, FUN = mean)
plotw <- aggregate(recall.word ~ condition*listlength, data = towse08, FUN = mean)
plotpause <- aggregate(pause ~ condition*listlength, data = towse08, FUN = mean)
# reorder data for plot
plotp <- plotp[c(2,1,4,3),]
plotw <- plotw[c(2,1,4,3),]
plotpause <- plotpause[c(2,1,4,3),]
### recreate barplot in Towse et al. (2008)
par(mfrow=c(1,3))
barplot(as.table(plotp$prep.int), main="Recall segments",
        xlab="Preparatory Interval", col=c("white", "lightgrey", "darkgrey", "black"),
        beside=TRUE, ylim = c(0,2))
barplot(as.table(plotw$recall.word),
        xlab="Words", col=c("white", "lightgrey", "darkgrey", "black"),
        ylim = c(0,2))
barplot(as.table(plotpause$pause),
        xlab="Pause", col = c("white", "lightgrey", "darkgrey", "black"),
        beside=TRUE, ylim = c(0,2))
legend(0.1,2, legend = c("LL 2 integrated", "LL 2 independent",
           "LL 3 integrated", "LL 2 independent", "LL = list length"), 
       col=c("white", "lightgrey", "darkgrey", "black"),
       fill=c("white", "lightgrey", "darkgrey", "black", NA))



