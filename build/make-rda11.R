## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
library(reshape2)
rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.1.Chunking/"

porchunk <- read.table(paste0(pth, "Portrat.Chunking.txt"),header=TRUE)
names(porchunk) <- c("subject", "S0",	"S1",	"S3",	"S5",	"F0",	"F1",	"F3",	"F5") #for some reason, the headers are not read correctly

porchunkmslow <- porchunk %>% gather("acronym","slow",S0:S5)%>%
  mutate(acronym=as.numeric(substr(acronym,2,10))) %>%
  select(-c(F0:F5))
porchunkmfast <- porchunk %>% gather("acronym","fast",F0:F5)%>%
  mutate(acronym=as.numeric(substr(acronym,2,10))) %>%
  select(-c(S0:S5))
porchunkfin <- inner_join(porchunkmslow,porchunkmfast,by=c("subject","acronym")) %>%
  gather("Pace","pc",slow:fast) %>% mutate(acronym=as.factor(acronym)) %>%
  mutate(Pace=as.factor(Pace))

portrat16 <- porchunkfin

portrat16$subject <- as.numeric(substr(portrat16$subject,3,10))

portrat16 <- portrat16 %>% dplyr::rename(subj = subject, condition = acronym,
                                  pace = Pace, acc = pc)
save(portrat16, file = "./pkg/data/portrat16.rda", compress = "xz")

## Reproduce Figure 20 in Oberauer et al. (2018)

pd <- aggregate(acc ~ pace+condition, data = portrat16, FUN = mean)
spacing <- c(0.2,0.2,0.7,0.2,0.7,0.2,0.7,0.2)
bp = barplot(pd$acc, col=c("darkgrey", "lightgrey", "darkgrey", "lightgrey",
                           "darkgrey", "lightgrey"), space = spacing,
             ylab="Number of Items Recalled", xlab="Acronym Condition",
             axisnames=F, ylim = c(0,7))

box()
axis(1, at = c(1.3,4.2,7.1,10), labels=c(0,1,3,5), cex.axis=0.7)
legend(9.5, 6.5, c("fast", "slow"), fill=c("darkgrey", "lightgrey"),
       horiz = F, cex = 0.6, yjust = 1, xjust = 0)


#
# bp = bargraph.CI(x.factor=acronym, group = Pace, response=pc, data=porchunkfin,
#                  cex.names=1.5, cex.lab = 1.5, ylim=c(2,6), cex.axis=1.5,
#                  ylab="Number of Items Recalled", xlab="Acronym Condition",
#                  legend = TRUE, cex.leg=1.5, yaxt="n",
#                  ci.fun=function(x) {c(mean(x) - 1.96*se(x), mean(x) + 1.96*se(x))})
#
# axis.break(2,2.5,style="slash")
# axis(2, at=c(3:6),lwd=1, lwd.ticks=1,cex.axis=1.5,las=1)
# box()


rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.1.Chunking/Thalmann.2018/"

### Exp1


memdat<-read.table(paste0(pth, "Chunking.Experiment1.dat"),
                   col.names=c("subject", "session", "trial", "chunk1", "chunk2",
                               "setsize1", "setsize2", "time_pres",
                               "time_rec", "seracc_p1", "seracc_p2", "seracc_p3",
                               "seracc_p4", "RT1", "RT2", "RT3", "RT4",
                               "freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"))
# meaning of columns:
# subject: id
# session: two experimental sessions (each à 1 h)
# trial: trial nr. note. there were two lists (upper and lower) per trial
# chunk1: type of the firstly presented list
# chunk2: type of the secondly presented list
# setsize1: size of the firstly presented list
# setsize2: size of the secondly presented list
# time_pres: presented as the first (== 1, upper) or the second (== 2, lower) list?
# time_rec: recalled first(== 1) or second (== 2)
# seracc_p1 - seracc_p4: serial recall accuracy for the respective list in forward order
# RT1 - RT4: reaction time for recalling the respective list in forward order
# freeacc_p1 - freeacc_p4: recall accuracy (lenient scoring) in forward order
# n.b. -999/999 are fillers for two-item lists

# NAs where no items were presented
not_pres <- memdat$seracc_p3 == 999
memdat$seracc_p3[not_pres] <- NA
memdat$seracc_p4[not_pres] <- NA
memdat$RT3[not_pres] <- NA
memdat$RT4[not_pres] <- NA
memdat$freeacc_p3[not_pres] <- NA
memdat$freeacc_p4[not_pres] <- NA

# recode time of recall for time_pres == 2 (wrongly coded in matlab code)
# first subset first and second rows of a trial
try1st <- seq(1,nrow(memdat), by = 2)
try2nd <- seq(2,nrow(memdat), by = 2)

first <- memdat[try1st,]
second <- memdat[try2nd,]
filter1 <- first$time_pres == 2 & first$time_rec ==1
filter2 <- first$time_pres == 2 & first$time_rec ==2
first$time_rec[filter1==1] <- 2
first$time_rec[filter2==1] <- 1
second$time_rec[filter1==1] <- 1
second$time_rec[filter2==1] <- 2

memdat_new <- rbind(first, second)
memdat <- memdat_new

# factors as factors
memdat$chunk1<-as.factor(memdat$chunk1)
memdat$chunk2<-as.factor(memdat$chunk2)
memdat$setsize1<-as.factor(memdat$setsize1)
memdat$setsize2<-as.factor(memdat$setsize2)
memdat$time_pres<-as.factor(memdat$time_pres)
memdat$time_rec<-as.factor(memdat$time_rec)

# integrate serial recall acc, free recall acc and rt in one df
memdat_sermem <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec","freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4","RT1", "RT2", "RT3", "RT4"), measured = c("seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4"))
memdat_sermem$seracc <- memdat_sermem$value
memdat_freemem <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec", "seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4", "RT1", "RT2", "RT3", "RT4"), measured = c("freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"))
memdat_freemem$freeacc <- memdat_freemem$value
memdat_rt <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec", "seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4", "freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"), measured = c("RT1", "RT2", "RT3", "RT4"))
memdat_rt$rt <- memdat_rt$value

memdat<-cbind(memdat_sermem[,1:9], memdat_sermem[,18], memdat_sermem[,20], memdat_rt[,20], memdat_freemem[,20])

memdat <- memdat %>% dplyr::rename(sp = `memdat_sermem[, 18]`)
memdat <- memdat %>% dplyr::rename(seracc = `memdat_sermem[, 20]`,
                                   freeacc = `memdat_freemem[, 20]`,
                                   rt = `memdat_rt[, 20]`)


names(memdat)[13] <- "freeacc"
names(memdat)[12] <- "rt"
names(memdat)[11] <- "seracc"
names(memdat)[10] <- "sp"
names(memdat)[4] <- "chunk_this"
names(memdat)[5] <- "chunk_other"
names(memdat)[6] <- "setsize_this"
names(memdat)[7] <- "setsize_other"

levels(memdat$sp) <- c("seracc_p1"="1", "seracc_p2"="2", "seracc_p3"="3", "seracc_p4"="4")
levels(memdat$time_rec) <- c("1"="Recall First", "2"="Recall Last")
levels(memdat$time_pres) <- c("1"="Presentation First", "2"="Presentation Last")
levels(memdat$chunk_this) <- c("0"="New List", "1"="Chunk")
levels(memdat$chunk_other) <- c("0"="New List", "1"="Chunk")

memdat <- memdat[!is.na(memdat$seracc),]

thalmann19a <- memdat %>% dplyr::rename(subj = subject)

save(thalmann19a, file = "./pkg/data/thalmann19a.rda", compress = "xz")

## Plot

pd <- aggregate(seracc ~ chunk_other+setsize_this+setsize_other,
                data = thalmann19a, FUN = mean)
pd$sso[pd$setsize_other == 2] <- 2
pd$sso[pd$setsize_other == 4] <- 4
p1 <- pd[which(pd$setsize_this == 2),]
p2 <- pd[which(pd$setsize_this == 4),]
par(mfrow=c(1,2))
plot(c(1,5), c(0.6,1.0), type = "n", xlab = "Size Other",
     ylab = "Proportion correct", main = "Size Tested = 2", xaxt = "n")
axis(side = 1, at = c(2,4), labels = c(2,4),
     cex.axis = 0.7)
lines(x = p1$sso[p1$chunk_other == "Chunk"],
      y = p1$seracc[p1$chunk_other == "Chunk"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = p1$sso[p1$chunk_other == "New List"],
      y = p1$seracc[p1$chunk_other == "New List"],
      type = "b", lty = 2, pch = 16, col = "red")
legend(1, 0.6, c("Chunk", "New List"), lty = 1:2,
       pch=15:17, col = c("blue","red"),
       horiz = F, cex = 0.6, yjust = 0, xjust = 0, title = "Type Other:")
plot(c(1,5), c(0.6,1.0), type = "n", xlab = "Size Other",
     ylab = "", main = "Size Tested = 4", xaxt = "n")
axis(side = 1, at = c(2,4), labels = c(2,4),
     cex.axis = 0.7)
lines(x = p2$sso[p2$chunk_other == "Chunk"],
      y = p2$seracc[p2$chunk_other == "Chunk"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = p2$sso[p2$chunk_other == "New List"],
      y = p2$seracc[p2$chunk_other == "New List"],
      type = "b", lty = 2, pch = 16, col = "red")

#################################################
### Exp 2
rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.1.Chunking/Thalmann.2018/"

data <- read.table(paste0(pth, "Chunking.Experiment2.dat"), header = F)
names(data) <- c("subject", "trial", "t.pres", "list.type", "list.id", "t.rec", "cond", "s_1", "s_2",
                 "s_3", "rt_1", "rt_2", "rt_3", "f_1", "f_2", "f_3", "t_1", "t_2", "t_3")
####### checking and preprocessing ###########
# do some checking
sum(data$list.type == 10)
sum(data$list.type == 3)
sum(data$list.type == 1)
sum(data$cond == 1 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 2 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 3 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 4 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 5 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
# --> looks good

# each id should be counted three times --> good
table(data$list.id)

chunks <- data[data$list.type == 10,]
nl1 <- data[data$list.type == 1,]
nl3 <- data[data$list.type == 3,]
# chunks 30 times in sublist position 1 and 30 times in sublist position 3
table(chunks$t.pres, chunks$t.rec)
# nl1 30 times in sublist position 1 and 30 times in sublist position 3
table(nl1$t.pres, nl1$t.rec)
# 3*30 in sublist position 1, 5*30 in sublist position 2, 3*30 in sublist position 3
table(nl3$t.pres, nl3$t.rec)

# mark at which sublist position ch3 OR! nl1 was presented in the trial
data$t.small.load <- 0
data$t.small.load[data$cond == 1] <- 1
data$t.small.load[data$cond == 2] <- 3
data$t.small.load[data$cond == 4] <- 1
data$t.small.load[data$cond == 5] <- 3

# was a chunk presented before or after a not-chunked list?
data$small.load.bef <- 0
data$small.load.bef[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] < data$t.pres[data$t.small.load != 0]
data$small.load.aft <- 0
data$small.load.aft <- as.numeric(data$t.small.load > data$t.pres)

# what is the position of the small load compared to a new list?
data$relpos.small.load <- 0
data$relpos.small.load[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] - data$t.pres[data$t.small.load != 0]

# introduce variable, whether whole sublist was recalled correctly
data$s_12 <- data$s_1
data$s_22 <- data$s_2
data$s_32 <- data$s_3
data$s_12[data$list.type == 1] <- 1
data$s_22[data$list.type == 1] <- 1
data$wholesl_1 <- data$s_12*data$s_22*data$s_32
data$wholesl_2 <- data$s_12*data$s_22*data$s_32
data$wholesl_3 <- data$s_12*data$s_22*data$s_32

# bring data in long format
data.long <- reshape(data, varying = c(8:19, 27:29), sep = "_", direction = 'long')
# Singletons were always presented at time == 3, exclude filler rows
incl <- as.logical((as.numeric(data.long$list.type == 1 & data.long$time < 3)-1)*-1)
data.long <- data.long[incl,]
# transposition 999 = 0
data.long$t[data.long$t == 999] <- 0
# introduce overall input serial position from 1-7/9
# therefore, first determine sublist length
data.long <- data.long[order(data.long$t.pres),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$inp.sp.all <- seq(1,7,1)
cond345$inp.sp.all <- seq(1,9,1)
# now overall output serial posiiton
data.long <- rbind(cond12, cond345)
data.long <- data.long[order(data.long$t.rec),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$out.sp.all <- seq(1,7,1)
cond345$out.sp.all <- seq(1,9,1)
data.long <- rbind(cond12, cond345)

# denote factors
vars = which(names(data.long) %in% c("subject", "cond", "t.pres", "list.type", "t.rec", "small.load.bef", "small.load.aft", "inp.sp.all", "out.sp.all"))
data.long[,vars] <- lapply(data.long[,vars], as.factor)

# name variables where necessary
levels(data.long$list.type) <- c("Singleton", "New List", "Chunk")
levels(data.long$cond) <- c("Singleton L1", "Singleton L3", "All New Lists", "Chunk L1", "Chunk L3")

# create var that states when singleton/chunk was recalled in a trial
data.long.small.out <- data.frame()
for (s in unique(data.long$subject)){
  tmp <- data.long[data.long$subject == s,]
  for (t in unique(tmp$trial)){
    trial <- tmp[tmp$trial == t,]

    # variable for chunk/singleton in list position 1
    trial$lp1.bef <- 0
    comp <- trial$t.rec[trial$t.pres==1][1]
    trial$lp1.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))

    # variable for chunk/singleton in list position 3
    trial$lp3.bef <- 0
    comp <- trial$t.rec[trial$t.pres==3][1]
    trial$lp3.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))

    data.long.small.out <- rbind(data.long.small.out, trial)
  }
}
tmp1 <- as.numeric(data.long.small.out$lp1.bef > 0 & data.long.small.out$cond == "Chunk L1")
tmp2 <- as.numeric(data.long.small.out$lp1.bef > 0 & data.long.small.out$cond == "Singleton L1")*2
tmp3 <- as.numeric(data.long.small.out$lp3.bef > 0 & data.long.small.out$cond == "Chunk L3")
tmp4 <- as.numeric(data.long.small.out$lp3.bef > 0 & data.long.small.out$cond == "Singleton L3")*2

data.long.small.out$rec.small.bef <- as.factor(tmp1 + tmp2 + tmp3 + tmp4)
levels(data.long.small.out$rec.small.bef) <- c("New List", "Chunk", "Singleton")


check_meanings <- aggregate(s ~ cond+list.type+inp.sp.all,
                            data = data.long.small.out,
                            FUN = mean)

### select the variables to save to package:

thalmann19b <- data.long %>% select(subject, trial, t.pres, list.type,
                                              t.rec, cond, s, f, t, rt, wholesl, inp.sp.all,
                                              out.sp.all)
thalmann19b <- thalmann19b %>% dplyr::rename(subj = subject, list.in.pos = t.pres,
                                             list.out.pos = t.rec, condition = cond,
                                             seracc = s, list.acc = wholesl,
                                             inpos = inp.sp.all, outpos = out.sp.all,
                                             freeacc = f, transposition = t)
thalmann19b <- thalmann19b %>% select(subj, trial, condition, list.type, list.in.pos,
                                      list.out.pos, inpos, outpos, list.acc, seracc,
                                      freeacc, transposition, rt)
thalmann19b$subj <- as.numeric(thalmann19b$subj)

thalmann19b$inpos <- as.numeric(thalmann19b$inpos)
thalmann19b$outpos <- as.numeric(thalmann19b$outpos)

save(thalmann19b, file = "./pkg/data/thalmann19b.rda", compress = "xz")

####################################
### Exp 3
rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.1.Chunking/Thalmann.2018/"

data <- read.table(paste0(pth,"Chunking.Experiment3.dat"), header = F)
names(data) <- c("subject", "trial", "t.pres", "list.type", "list.id", "t.rec", "cond", "s_1", "s_2",
                 "s_3", "rt_1", "rt_2", "rt_3", "f_1", "f_2", "f_3", "t_1", "t_2", "t_3")
####### checking and preprocessing ###########
# do some checking
sum(data$list.type == 10)
sum(data$list.type == 3)
sum(data$list.type == 1)
sum(data$cond == 1 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 2 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 3 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 4 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 5 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
# --> looks good

table(data$list.id)

chunks <- data[data$list.type == 10,]
nl1 <- data[data$list.type == 1,]
nl3 <- data[data$list.type == 3,]
# chunks 30 times in sublist position 1 and 30 times in sublist position 3
table(chunks$t.pres, chunks$t.rec)
# nl1 30 times in sublist position 1 and 30 times in sublist position 3
table(nl1$t.pres, nl1$t.rec)
# 3*30 in sublist position 1, 5*30 in sublist position 2, 3*30 in sublist position 3
table(nl3$t.pres, nl3$t.rec)

# mark at which sublist position ch3 OR! nl1 was presented in the trial
data$t.small.load <- 0
data$t.small.load[data$cond == 1] <- 1
data$t.small.load[data$cond == 2] <- 3
data$t.small.load[data$cond == 4] <- 1
data$t.small.load[data$cond == 5] <- 3

# was a chunk presented before or after a not-chunked list?
data$small.load.bef <- 0
data$small.load.bef[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] < data$t.pres[data$t.small.load != 0]
data$small.load.aft <- 0
data$small.load.aft <- as.numeric(data$t.small.load > data$t.pres)

# what is the position of the small load compared to a new list?
data$relpos.small.load <- 0
data$relpos.small.load[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] - data$t.pres[data$t.small.load != 0]

# introduce variable, whether whole sublist was recalled correctly
data$s_12 <- data$s_1
data$s_22 <- data$s_2
data$s_32 <- data$s_3
data$s_12[data$list.type == 1] <- 1
data$s_22[data$list.type == 1] <- 1
data$wholesl_1 <- data$s_12*data$s_22*data$s_32
data$wholesl_2 <- data$s_12*data$s_22*data$s_32
data$wholesl_3 <- data$s_12*data$s_22*data$s_32

# bring data in long format
data.long <- reshape(data, varying = c(8:19, 27:29), sep = "_", direction = 'long')
# Singletons were always presented at time == 3, exclude filler rows
incl <- as.logical((as.numeric(data.long$list.type == 1 & data.long$time < 3)-1)*-1)
data.long <- data.long[incl,]
# transposition 999 = 0
data.long$t[data.long$t == 999] <- 0
# introduce overall input serial position from 1-7/9
# therefore, first determine sublist length
data.long <- data.long[order(data.long$t.pres),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$inp.sp.all <- seq(1,7,1)
cond345$inp.sp.all <- seq(1,9,1)
# now overall output serial posiiton
data.long <- rbind(cond12, cond345)
data.long <- data.long[order(data.long$t.rec),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$out.sp.all <- seq(1,7,1)
cond345$out.sp.all <- seq(1,9,1)
data.long <- rbind(cond12, cond345)

# denote factors
vars = which(names(data.long) %in% c("subject", "cond", "t.pres", "list.type", "t.rec",
                                     "small.load.bef", "small.load.aft", "inp.sp.all", "out.sp.all"))
data.long[,vars] <- lapply(data.long[,vars], as.factor)

# name variables where necessary
levels(data.long$list.type) <- c("Singleton", "New List", "Chunk")
levels(data.long$cond) <- c("Singleton L1", "Singleton L3", "All New Lists", "Chunk L1", "Chunk L3")

# create var that states when singleton/chunk was recalled in a trial
data.long.small.out <- data.frame()

# aggregate before plotting! for serial recall, free recall and transpositions
recall.agg <- aggregate(cbind(s,f,t,wholesl) ~ subject + cond + t.pres + list.type + t.rec + small.load.bef +
                          small.load.aft + relpos.small.load + inp.sp.all + out.sp.all, data = data.long, FUN = mean)
rt.agg <- aggregate(rt ~ subject + cond + t.pres + list.type + t.rec + small.load.bef +
                      small.load.aft + relpos.small.load + inp.sp.all + out.sp.all, data = data.long, FUN = mean)


### transform for package

thalmann19c <- data.long %>% select(subject, trial, t.pres, list.type,
                                    t.rec, cond, s, f, t, rt, wholesl, inp.sp.all,
                                    out.sp.all)
thalmann19c <- thalmann19c %>% dplyr::rename(subj = subject, list.in.pos = t.pres,
                                             list.out.pos = t.rec, condition = cond,
                                             seracc = s, list.acc = wholesl,
                                             inpos = inp.sp.all, outpos = out.sp.all,
                                             freeacc = f, transposition = t)
thalmann19c <- thalmann19c %>% select(subj, trial, condition, list.type, list.in.pos,
                                      list.out.pos, inpos, outpos, list.acc, seracc,
                                      freeacc, transposition, rt)
thalmann19c$subj <- as.numeric(thalmann19c$subj)

thalmann19c$inpos <- as.numeric(thalmann19c$inpos)
thalmann19c$outpos <- as.numeric(thalmann19c$outpos)

save(thalmann19c, file = "./pkg/data/thalmann19c.rda", compress = "xz")

###########################################

### Exp 4
rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.1.Chunking/Thalmann.2018/"

### subject nr. 9 has begun twice --> do not forget to delete first part of that data in second attempt before further processing
data <- read.table(paste0(pth,"Chunking.Experiment4.dat"), header = F)
names(data) <- c("subject", "trial", "chunk.pres", "t.pres", "listtype", "stim.id", "t.rec", "t.ch.pres", "s_1", "s_2",
                 "s_3", "rt_1", "rt_2", "rt_3", "f_1", "f_2", "f_3", "t_1", "t_2", "t_3")
timepoint_chunk <- data$t.ch.pres
# save(timepoint_chunk, file = "timepoint.chunk.Rda")

# there should be 90 chunks --> good
nchunks <- nrow(subset(data, chunk.pres == 1 & listtype == 1))

# each id should be counted three times --> good
chunks <- subset(data, listtype == 1)
nl <- subset(data, listtype == 2)
data <- rbind(chunks, nl)
table(data$stim.id)

# each chunk/nl should be presented equally often in 1st, 2nd or 3rd position --> good
table(chunks$t.pres)
table(nl$t.pres)

# bring data in long format
data.long <- reshape(data, varying = 9:20, sep = "_", direction = 'long')
# introduce overall input serial position from 1-9
data.long$inp.sp.all <- (data.long$t.pres-1)*3+data.long$time
data.long$out.sp.all <- (data.long$t.rec-1)*3+data.long$time
# define factors as factors
data.long$chunk.pres <- as.factor(data.long$chunk.pres)
levels(data.long$chunk.pres) <- c("No Chunk Present", "Chunk Present")
data.long$listtype <- as.factor(data.long$listtype)
levels(data.long$listtype) <- c("Chunk", "New List")
data.long$t.rec <- as.factor(data.long$t.rec)
data.long <- data.long[order(data.long$trial, data.long$t.pres),]
data.long$t.pres <- as.factor(data.long$t.pres)
data.long$time <- as.factor(data.long$time)
data.long$t.ch.pres <- as.factor(data.long$t.ch.pres)

# save df for later lenient scoring analysis
data.free <- data.long

# create var that states when singleton/chunk was recalled in a trial
data.long.small.out <- data.frame()
for (s in unique(data.long$subject)){
  tmp <- data.long[data.long$subject == s,]
  for (t in unique(tmp$trial)){
    trial <- tmp[tmp$trial == t,]

    trial$cond <- 0
    trial$cond <- trial$t.pres[trial$listtype=="Chunk"][1]
    data.long.small.out <- rbind(data.long.small.out, trial)

  }
}
data.long.small.out$cond <- as.numeric(data.long.small.out$cond)
data.long.small.out$cond[is.na(data.long.small.out$cond)]<-4
unique(data.long.small.out$cond)



### transform for package

thalmann19d <- data.long.small.out %>% select(subject, trial, t.pres, listtype,
                                    t.rec, cond, s, f, t, rt, inp.sp.all,
                                    out.sp.all)
thalmann19d <- thalmann19d %>% dplyr::rename(subj = subject, list.in.pos = t.pres,
                                             list.out.pos = t.rec, condition = cond,
                                             seracc = s, list.type = listtype,
                                             inpos = inp.sp.all, outpos = out.sp.all,
                                             freeacc = f, transposition = t)
thalmann19d <- thalmann19d %>% select(subj, trial, condition, list.type, list.in.pos,
                                      list.out.pos, inpos, outpos, seracc,
                                      freeacc, transposition, rt)
thalmann19d$subj <- as.numeric(thalmann19d$subj)

thalmann19d$inpos <- as.numeric(thalmann19d$inpos)
thalmann19d$outpos <- as.numeric(thalmann19d$outpos)

thalmann19d$condition[thalmann19d$condition == 1] <- "Chunk L1"
thalmann19d$condition[thalmann19d$condition == 2] <- "Chunk L2"
thalmann19d$condition[thalmann19d$condition == 3] <- "Chunk L3"
thalmann19d$condition[thalmann19d$condition == 4] <- "All New Lists"

thalmann19d$condition <- as.factor(thalmann19d$condition)

save(thalmann19d, file = "./pkg/data/thalmann19d.rda", compress = "xz")

##### Word Frequencies

rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.3.Lexicality.Frequency/"

ntrials <- 60
nsubj <- 91

Demographics <- as.data.frame(matrix(NA, nsubj, 3))
names(Demographics) <- c("id", "Sex", "Age")
Materials <- as.data.frame(matrix(NA, nsubj*ntrials, 9))
names(Materials) <- c("id", "trial", "Freq", "P1", "P2", "P3", "P4", "P5", "P6")
Order <- Materials
Responses <- Materials
Accuracy <- as.data.frame(matrix(NA, nsubj*ntrials, 10))
names(Accuracy) <- c("id", "trial", "Task", "Cond", "P1", "P2", "P3", "P4", "P5", "P6")

for (id in 1:nsubj) {
  demo <- read_excel(paste0(pth,"Quinlan.2017.Exp4.xlsx"), sheet=id+2, range="A2:A3")
  materials <- read_excel(paste0(pth,"Quinlan.2017.Exp4.xlsx"), sheet=id+2, range="A4:G64")
  order <- read_excel(paste0(pth,"Quinlan.2017.Exp4.xlsx"), sheet=id+2, range="A66:G126")
  responses <- read_excel(paste0(pth,"Quinlan.2017.Exp4.xlsx"), sheet=id+2, range="A128:G188")
  accuracy <- read_excel(paste0(pth,"Quinlan.2017.Exp4.xlsx"), sheet=id+2, range="H128:O188")
  Demographics[id, ] <- cbind(id, demo)
  rows <- ((id-1)*ntrials + 1):(id*ntrials)
  Materials[rows, 1] <- id
  Materials[rows, 2] <- 1:ntrials
  Materials[rows, 3:9] <- materials
  Responses[rows, 1] <- id
  Responses[rows, 2] <- 1:ntrials
  Responses[rows, 3:9] <- responses
  Accuracy[rows, 1] <- id
  Accuracy[rows, 2] <- 1:ntrials
  Accuracy[rows, 3:10] <- accuracy
  Order[rows, 1] <- id
  Order[rows, 2] <- 1:ntrials
  Order[rows, 3:9] <- order
}

# Order: for serial reconstruction, order in which the words were arranged for reconstruction (for serial recall, this appears to code accuracy: 1 = correct, 0 = incorrect word in each list position)
# Response: for serial reconstruction, position in the array that was selected (if it matches "order", the response is correct)
# Accuracy: Task: 1 = reconstruction, 2 = recall, Condition: 1- High F reconstruction, 2 - Low F reconstruction, 3 - High F recall, 4, Low F recall. The remaining 6 columns are the accuracy in each serial position

aggdat <- aggregate(cbind(P1,P2,P3,P4,P5,P6) ~ id+Cond, data=Accuracy, FUN=mean)

quin <- Accuracy

idvec <- sort(unique(quin$id))
trialvec <- sort(unique(quin$trial))

quin[11:16] <- NA
colnames(quin)[11:16] <- c("stim1", "stim2", "stim3", "stim4", "stim5", "stim6")
quin[17:22] <- NA
colnames(quin)[17:22] <- c("resp1", "resp2", "resp3", "resp4", "resp5", "resp6")

for (subj in idvec) {
  for (tr in trialvec) {
    cc <- quin$id == subj & quin$trial == tr
    task <- quin$Task[cc]
    if (task == 1) {
      quin[cc, 5:10] <- Order[cc, 4:9]
      quin[cc, 11:16] <- Materials[cc, 4:9]
      quin[cc, 17:22] <- Responses[cc, 4:9]
    } else {
      quin[cc, 11:16] <- Materials[cc, 4:9]
      quin[cc, 17:22] <- Responses[cc, 4:9]
    }
  }
}

## get into long format

quin.long <- gather(quin, key = "serpos", value = "acc", 5:10)
quin.stims <- gather(quin, key = "serpos", value = "stim", 11:16)
quin.resps <- gather(quin, key = "serpos", value = "resp", 17:22)

quin.long <- quin.long %>% select(id, trial, Task, Cond, serpos, acc)

quin.long <- cbind(quin.long, quin.stims$serpos, quin.stims$stim, quin.resps$serpos,
                   quin.resps$resp)

quin.long$serpos <- as.numeric(substr(quin.long$serpos,2,3))
quin.long$`quin.stims$serpos` <- as.numeric(substr(quin.long$`quin.stims$serpos`, 5,5))
quin.long$`quin.resps$serpos` <- as.numeric(substr(quin.long$`quin.resps$serpos`, 5,5))

check1 <- quin$id[quin.long$serpos != quin.long$`quin.stims$serpos`]
check2 <- quin$id[quin.long$serpos != quin.long$`quin.resps$serpos`]

quin.long <- quin.long %>% dplyr::rename(stim = `quin.stims$stim`, resp = `quin.resps$resp`)
quin.long <- quin.long %>% select(id, trial, Task, Cond, serpos, stim, resp, acc)

### get the accuracy of the serial reproduction task.

quin.long$resp <- as.character(quin.long$resp)
quin.long$acc <- as.character(quin.long$acc)
quin.long$stim <- as.character(quin.long$stim)

quin.long$pc <- quin.long$acc
quin.long$acc[quin.long$Task == 1 & quin.long$resp != quin.long$acc] <- "0"
quin.long$acc[quin.long$Task == 1 & quin.long$resp == quin.long$acc] <- "1"

quin.long$acc <- as.numeric(quin.long$acc)

quin.long <- quin.long %>% dplyr::rename(subj = id, task = Task, condition = Cond)

quin.long$resp[quin.long$acc == 1 & quin.long$task == 1] <- quin.long$stim[quin.long$acc == 1 & quin.long$task == 1]

### name tasks and conditions
### Task: 1 = reconstruction, 2 = recall, Condition: 1- High F reconstruction, 2 - Low F reconstruction, 3 - High F recall, 4, Low F recall.

quin.long$task[quin.long$task == 1] <- "reconstruction"
quin.long$task[quin.long$task == 2] <- "recall"

quin.long$frequency[quin.long$condition == 1] <- "high"
quin.long$frequency[quin.long$condition == 2] <- "low"
quin.long$frequency[quin.long$condition == 3] <- "high"
quin.long$frequency[quin.long$condition == 4] <- "low"

quin.long$condition[quin.long$condition == 1] <- "Hi-Recon"
quin.long$condition[quin.long$condition == 2] <- "Lo-Recon"
quin.long$condition[quin.long$condition == 3] <- "Hi-Recall"
quin.long$condition[quin.long$condition == 4] <- "Lo-Recall"


quin.long$condition <- as.factor(quin.long$condition)
quin.long$task <-  as.factor(quin.long$task)
quin.long$frequency <-  as.factor(quin.long$frequency)



quinlan17 <- quin.long %>% select(subj, trial, task, frequency, condition, serpos,
                                  stim, resp, acc)

save(quinlan17, file = "./pkg/data/quinlan17.rda", compress = "xz")

### Reconstruct Figure 4 in Quinlan, Roodenrys, & Miller (2017)

par(mfrow=c(1,1))
pd <- aggregate(acc ~ serpos+condition, data = quinlan17, FUN = mean)
plot(c(1,6), c(0,1.0), type = "n", xlab = "Set Size",
     ylab = "Proportion correct", main = "Word Frequency Effect in Serial Recall
     and Serial Reconstruction", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = c(1,2,3,4,5,6),
     cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "Hi-Recall"],
      y = pd$acc[pd$condition == "Hi-Recall"],
      type = "b", lty = 1, pch = 15)
lines(x = pd$serpos[pd$condition == "Lo-Recall"],
      y = pd$acc[pd$condition == "Lo-Recall"],
      type = "b", lty = 2, pch = 16)
lines(x = pd$serpos[pd$condition == "Hi-Recon"],
      y = pd$acc[pd$condition == "Hi-Recon"],
      type = "b", lty = 3, pch = 17)
lines(x = pd$serpos[pd$condition == "Lo-Recon"],
      y = pd$acc[pd$condition == "Lo-Recon"],
      type = "b", lty = 4, pch = 18)
legend(1, 0, c("Hi-Recall", "Lo-Recall", "Hi-Recon", "Lo-Recon"), lty = 1:4,
       pch=15:18,
       horiz = F, cex = 0.6, yjust = 0, xjust = 0)



############
#### Hebb Effect Page
rm(list=ls())
############### BM11 Effects of Knowledge #####################
pth  <- "BenchmarksWM.Data/BM11.5.HebbEffect/"

#read subject-level data
hebb<- read.table(paste0(pth,"Page.Hebb.txt"), header=T)
names(hebb) <- c("subject","cond","order","supnosup",paste("t",c(1:16),sep=""))

###
# Bakeman & McArthur correction (for long data): id = column with subject id, dv = column with dependent variable
BakemanL <- function (data, id=1, dv=2) {
  idvar <- data[,id]
  subjMeans <- aggregate(x=data[,dv], by=list(data[,id]), FUN=mean)
  names(subjMeans) <- c(id, dv)
  ids <- unique(idvar)
  corrdata <- data
  for (ii in 1:length(ids)) {
    corrdata[data[,id]==ids[ii],dv] <- corrdata[data[,id]==ids[ii],dv] - subjMeans[subjMeans[,id]==ids[ii],2] + mean(subjMeans[,2])
  }
  return(corrdata)
}
hebbsubmeans <- hebb %>% gather("trial","pc",t1:t16) %>% mutate(trial=as.numeric(substr(trial,2,10))) %>%
  group_by(cond, supnosup, subject,trial) %>% summarise(pc = mean(pc))
hebbsubmeans <- BakemanL(as.data.frame(hebbsubmeans), id="subject", dv="pc")
hebbmeans <- aggregate(pc ~ cond + supnosup + trial, data=hebbsubmeans, FUN=mean)
hebbSE <- aggregate(pc ~ cond + supnosup + trial, data=hebbsubmeans, FUN=function(x) sd(x)/sqrt(length(x)))

# #now do some plotting
# bgk <- c("black","gray","black","black","white")
# p2f<-0
# if (p2f) {
#   pdf(file="pageHebb.pdf",height=12,width=10)
# } else {x11(height=12,width=10)}
# par(mfrow=c(2,1))
# par(mar=c(4,4,2,2))
# for (sns in c("ns","su")) {
#   if (sns=="ns") {
#     t4p <- "Silent"
#   } else {
#     t4p <- "Articulation"
#   }
#   plot(0,0, xlim=c(0.5,16.5),ylim=c(0.3,.8), type="n", las=1, xaxt="n",
#        xlab="Trial", ylab="Proportion Correct",cex.lab=1,cex.axis=1)
#
#   allconds <- unique(hebbmeans$cond)
#   for (k in c(2:length(allconds))) {  #run over cond (but not FillA)
#     tbp <- filter(hebbmeans,cond==allconds[k] & supnosup==sns)
#     se <- filter(hebbSE,cond==allconds[k] & supnosup==sns)
#     tbpspos <- tbp$trial
#     #lines(tbpspos,tbp$pc,lwd=2,lty=k-1)
#     #points(tbpspos,tbp$pc,pch=20+k,bg=bgk[k],cex=1)
#     par(new=T)
#     errbar(tbpspos, y=tbp$pc, yplus=tbp$pc+1.96*se$pc, yminus=tbp$pc-1.96*se$pc, type="b", las=1,
#            xlim=c(0.5,16.5),ylim=c(0.3,.8), xlab="", ylab="", pch=20+k, bg=bgk[k], errbar.col=bgk[k], cex=1)
#   }
#   axis(1, at=c(1:16),lwd=1, lwd.ticks=1,cex.axis=1)
#   legend(4,.45,c("Filler","Repeating"),lty=c(1:2),pch=20+c(2:length(allconds)),pt.bg=bgk[2:3],cex=1,pt.cex=1)
#   text(2.5,.75,t4p,cex=1)
# }


### Experiment 1

hebb.long <- gather(hebb, key = "trial", value = "acc", 5:20)
hebb.long$trial <- as.numeric(substr(hebb.long$trial,2,3))

page06 <- hebb.long %>% dplyr::rename(subj = subject, AS = supnosup, condition = cond)
page06 <- page06 %>% select(subj, trial, condition, AS, acc)

save(page06, file = "./pkg/data/page06.rda", compress = "xz")

pd <- aggregate(acc ~ condition + AS + trial, data=page06, FUN=mean)
plot(c(1,16), c(0.3,0.8), type = "n", xlab = "Trial",
     ylab = "Proportion correct", main = "Hebb Repitition Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
     cex.axis = 0.7)
lines(x = pd$trial[pd$condition == "FillB" & pd$AS == "ns"],
      y = pd$acc[pd$condition == "FillB" & pd$AS == "ns"],
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$trial[pd$condition == "HebbEffect" & pd$AS == "ns"],
      y = pd$acc[pd$condition == "HebbEffect" & pd$AS == "ns"],
      type = "b", lty = 2, pch = 16, col = "blue")
lines(x = pd$trial[pd$condition == "FillB" & pd$AS == "su"],
      y = pd$acc[pd$condition == "FillB" & pd$AS == "su"],
      type = "b", lty = 1, pch = 17, col = "red")
lines(x = pd$trial[pd$condition == "HebbEffect" & pd$AS == "su"],
      y = pd$acc[pd$condition == "HebbEffect" & pd$AS == "su"],
      type = "b", lty = 1, pch = 18, col = "red")
legend(16, 0.3, c("Filler", "Repeating", "Filler - AS", "Repeating - AS"), lty = c(2,1,2,1),
       pch=15:18, col = c("blue", "blue", "red", "red"),
       horiz = F, cex = 0.6, yjust = 0, xjust = 1)





















