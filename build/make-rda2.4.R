## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM2.1 #####################
pth  <- "BenchmarksWM.Data/BM2.5.PresTime/"

### read in data from Tan and Ward (2008)

fnam <- paste0(pth, "Tan.Ward.2008.dat")
dat <- read.table(fnam, header = F)
names(dat) <- c("subj", "trial", "ptime", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6")

d_long = gather(dat, key="serpos", value="corr", 4:9)

d_long$serpos[d_long$serpos == "sp1"] <- 1
d_long$serpos[d_long$serpos == "sp2"] <- 2
d_long$serpos[d_long$serpos == "sp3"] <- 3
d_long$serpos[d_long$serpos == "sp4"] <- 4
d_long$serpos[d_long$serpos == "sp5"] <- 5
d_long$serpos[d_long$serpos == "sp6"] <- 6

tan08 <- d_long
save(tan08, file="./pkg/data/tan08.rda")

plotd <- aggregate(corr ~ serpos*ptime, data = tan08, FUN = mean)
plot(c(1,6), c(0,1), type = "n", xlab = "Serial Position",
     ylab = "Proportion Correct", main = "Serial Recall as an Effect of 
     Presentation Timing", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = T)
lines(x = plotd$serpos[plotd$ptime == 1], y = plotd$corr[plotd$ptime == 1], type = "b", lty = 1, pch = 15)
lines(x = plotd$serpos[plotd$ptime == 2.5], y = plotd$corr[plotd$ptime == 2.5], type = "b", lty = 2, pch = 16)
lines(x = plotd$serpos[plotd$ptime == 5], y = plotd$corr[plotd$ptime == 5], type = "b", lty = 3, pch = 17)
legend(4, 0.6, c("1 s / item", "2.5 s / item", "5 s / item"), lty = 1:3, pch=15:17, yjust=0)


### Read in data from Bays et al. (2011)

fnam <- paste0(pth, "Bays.2011.dat")
datb <- read.table(fnam, header = F)
names(datb) <- c("subj", "size", "ptime", "itemsep", "angle", "response", "error")

datb$dev <- pi*datb$error/180
datb$ptime <- datb$ptime/1000

### create unique subject IDs for each participant
datb$subj[datb$size == 2] <- datb$subj[datb$size == 2]+8
datb$subj[datb$size == 4] <- datb$subj[datb$size == 4]+16
datb$subj[datb$size == 6] <- datb$subj[datb$size == 6]+24

bays11 <- datb %>% select(subj, size, ptime, error, dev)
save(bays11, file="./pkg/data/bays11.rda")

### Reproduce Figure 6A in Oberauer et al. (2018)

library("circular")
### get circular mean in rad
pd <- aggregate(dev ~ ptime*size, data = bays11, FUN = sd.circular)
pd$devrad <- 1/pd$dev
plot(c(0.0,1.0), c(0,6), type = "n", xlab = "Presentation Time (s)",
     ylab = "Mean Precision (1/rad)", main = "Reproduction of Colors", xaxt = "n")
axis(side = 1, at = c(0,0.2,0.4,0.6,0.8,1.0), labels = T)
lines(x = pd$ptime[pd$size == 1], y = pd$devrad[pd$size == 1], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$ptime[pd$size == 2], y = pd$devrad[pd$size == 2], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$ptime[pd$size == 4], y = pd$devrad[pd$size == 4], 
      type = "b", lty = 3, pch = 17)
lines(x = pd$ptime[pd$size == 6], y = pd$devrad[pd$size == 6], 
      type = "b", lty = 4, pch = 18)
legend(1.0, 0, c("1", "2", "3", "4"), lty = 1:4, pch=15:18, title = "Set size:",
       horiz = T, cex = 0.6, yjust = 0, xjust = 1)

#par(mfrow=c(1,1))

### read in data from grenfell et al. (2013)

fnam <- paste0(pth, "Grenfell-EssamWard&Tan(2013)JEPLM&C.xls")
# Load data for free-recall experiment
data = as.data.frame(read_excel(fnam))  
# include: 1=yes, 2=no; Suppression: "AS" = articulatory suppression, "NO AS" = no articulatory suppression

# add numerical presentation time (in s)
data$prestime <- 0
data[data$blockrate=="f",]$prestime <- 0.5
data[data$blockrate=="m",]$prestime <- 1
data[data$blockrate=="s",]$prestime <- 3

## get the general proportion correct (regardless of order)

data$corr <- 0
data$corr[!is.na(data$outputorder)] <- 1

d <- data %>% select(Include, subj, trialnumber, Suppression, prestime, 
                     length, serpos, FRcorrect, corr)

grenfell13 <- d
save(grenfell13, file="./pkg/data/grenfell13.rda")

## plots
dincl <- grenfell13[which(grenfell13$Include == 1),]
#dAS <- dincl[which(dincl$Suppression == "AS"),] 
#dSilent <- dincl[which(dincl$Suppression == "No AS"),]
###################
d_FR <- dincl[which(dincl$serpos == 1),]
dAS <- d_FR[which(d_FR$Suppression == "AS"),] 
dSilent <- d_FR[which(d_FR$Suppression == "No AS"),]

par(mfrow=c(1,2))
#silent
splot <- aggregate(FRcorrect ~ length*prestime, data = dSilent, FUN = mean)
plot(c(2,12), c(0,1.1), type = "n", xlab = "List length",
     ylab = "P(first item recalled correctly)", main = "Silent", xaxt = "n")
axis(side = 1, at = c(2,4,5,6,7,8,10,12), labels = T)
lines(x = splot$length[splot$prestime == 0.5], 
      y = splot$FRcorrect[splot$prestime == 0.5], 
      type = "b", lty = 1, pch = 15)
lines(x = splot$length[splot$prestime == 1], 
      y = splot$FRcorrect[splot$prestime == 1], 
      type = "b", lty = 2, pch = 16)
lines(x = splot$length[splot$prestime == 3], 
      y = splot$FRcorrect[splot$prestime == 3], 
      type = "b", lty = 3, pch = 17)
legend(2, 0, c("0.5 s / item", "1 s / item", "3 s / item"), lty = 1:3, pch=15:17,
       title = "Pres. Time:", horiz = F, cex = 0.5, yjust = 0, xjust = 0)

ASplot <- aggregate(FRcorrect ~ length*prestime, data = dAS, FUN = mean)
plot(c(2,12), c(0,1.1), type = "n", xlab = "List length",
     ylab = "P(first item recalled correctly)", main = "Articulatory Suppression", xaxt = "n")
axis(side = 1, at = c(2,4,5,6,7,8,10,12), labels = T)
lines(x = ASplot$length[ASplot$prestime == 0.5], 
      y = ASplot$FRcorrect[ASplot$prestime == 0.5], 
      type = "b", lty = 1, pch = 15)
lines(x = ASplot$length[ASplot$prestime == 1], 
      y = ASplot$FRcorrect[ASplot$prestime == 1], 
      type = "b", lty = 2, pch = 16)
lines(x = ASplot$length[ASplot$prestime == 3], 
      y = ASplot$FRcorrect[ASplot$prestime == 3], 
      type = "b", lty = 3, pch = 17)



#####################
## get into normal long format
## maybe don't do this to spear time.
datal <- data %>% select(-outputorder, -word)
dl <- spread(datal, key="serpos", value = "corr")
dl$acc <- rowMeans(subset(dl, select = colnames(dl[11:22])), na.rm = T)
dl <- dl %>% select(subj, Include, Suppression, trialnumber, length, FRcorrect, 
                    prestime, acc)





### read in ricker17 exp 2

fnam <- paste0(pth, "RickerHardman2017Exp2.dat")
dat <- read.table(fnam, header = T)
## x are presented angles, y are responses in angles, err is the precision error

d_short_err <- dat %>% select(sub, block, trial, ctime, err1, err2, err3, err4)
d_short_rt <-  dat %>% select(sub, block, trial, ctime, rt1, rt2, rt3, rt4)
d_err <-  gather(d_short_err, key = "serpos", value = "error", 5:8)
d_rt <- gather(d_short_rt, key = "serpos", value = "rt", 5:8)

d <- cbind(d_err, d_rt$rt)
names(d)[names(d) == "d_rt$rt"] <- "rt"

d$serpos[d$serpos == "err1"] <- 1
d$serpos[d$serpos == "err2"] <- 2
d$serpos[d$serpos == "err3"] <- 3
d$serpos[d$serpos == "err4"] <- 4

# trial numbers!
d$trial <- (d$block-1)*max(d$trial) + d$trial

d <- d %>% dplyr::rename(subj = sub)

ricker17 <- d
save(ricker17, file="./pkg/data/ricker17.rda")

### plotting:

### Vague reproduction of Figure 5 in Ricker & Hardman (2017)
ricker17$error <- abs(ricker17$error)
dp <- aggregate(error ~ serpos*ctime, data = ricker17, FUN = mean)

par(mfrow=c(1,1))
plot(c(0.2,1.0), c(0,55), type = "n", xlab = "Condolidation time in seconds",
     ylab = "Precision error in degrees", 
     main = "Serial Position and Consolidation Time", xaxt = "n")
axis(side = 1, at = c(0.2,0.4,0.6,0.8,1.0), labels = T)

lines(x = dp$ctime[dp$serpos == 1], 
      y = dp$error[dp$serpos == 1], 
      type = "l", lty = 1)
lines(x = dp$ctime[dp$serpos == 2], 
      y = dp$error[dp$serpos == 2], 
      type = "l", lty = 2)
lines(x = dp$ctime[dp$serpos == 3], 
      y = dp$error[dp$serpos == 3], 
      type = "l", lty = 3)
lines(x = dp$ctime[dp$serpos == 4], 
      y = dp$error[dp$serpos == 4], 
      type = "l", lty = 4)
legend(1.0, 0, c("1", "2", "3", "4"), lty = 1:4,
       title = "Serial Position", horiz = F, cex = 0.5, yjust = 0, xjust = 1)



