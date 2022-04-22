## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM2.1 #####################
pth  <- "BenchmarksWM.Data/BM2.1.RetentionInterval/"


############### Floden (2000) #####################

fnam <- paste0(pth, "Floden.BrownPeterson.xlsx")

temp <- read_excel(fnam,sheet="Exp2_Data")
temp$SUBJECT[temp$SUBJECT == -3] <- 3

dat2 <- subset(temp, select = c("SUBJECT", "GROUP", "AGE", "PROB_0", "PROB_3", "PROB_18", "PROB_36", "PROB_60"))

dat2 <- dat2 %>% rename(subj = SUBJECT) %>% rename(group = GROUP) %>% rename(age = AGE)
dat2$subj[dat2$group == 2] <- dat2$subj[dat2$group == 2] + 25
dat2$group[dat2$group == 1] <- 'old'
dat2$group[dat2$group == 2] <- 'young'

## gather and make long
d2_long <- gather(dat2, key = "RI", value = "acc", 4:8)

## make RIs numeric:
d2_long$RI[d2_long$RI == "PROB_0"] <- 0
d2_long$RI[d2_long$RI == "PROB_3"] <- 3
d2_long$RI[d2_long$RI == "PROB_18"] <- 18
d2_long$RI[d2_long$RI == "PROB_36"] <- 36
d2_long$RI[d2_long$RI == "PROB_60"] <- 60
d2_long$RI <- as.numeric(d2_long$RI)

floden10 <- d2_long
save(floden10, file="./pkg/data/floden10.rda", compress = "xz")

### create plot as in something

plotd <- aggregate(acc ~ RI*group, data = floden10, FUN = mean)
par(mfrow=c(1,1))

plot(c(0,60), c(0,1), type = "n", xlab = "Retention Interval (s)",
     ylab = "Proportion Correct", main = "Age Differences in the Brown-Peterson
     Task", xaxt = "n")
axis(side = 1, at = c(0,3,18,36,60), labels = T)
young <- subset(plotd, plotd$group == "young")
lines(x = young$RI, y = young$acc, type = "b", lty = 1, pch = 15)
old <- subset(plotd, plotd$group == "old")
lines(x = old$RI, y = old$acc, type = "b", lty = 2, pch = 16)

legend(1, 0.1, c("younger adults", "older adults"), lty = 1:2, pch=15:16, yjust=0)


### read in Lewandowsky et al. (2010)
fnam <- paste0(pth, "Lewandowsky.2010.E2.dat")
lsky10 <- read.table(fnam,header=FALSE,stringsAsFactors = FALSE)
names(lsky10) <- c("subject", "dummy",  "cond", "trial",
                   paste("li",c(1:5),sep=""),paste("distrt",c(1:5),sep=""),
                   paste("re",c(1:5),sep=""),paste("rt",c(1:5),sep=""))

lsky10resp <- lsky10 %>% mutate(resp1=as.numeric(li1==re1), resp2=as.numeric(li2==re2),
                                resp3=as.numeric(li3==re3), resp4=as.numeric(li4==re4), resp5=as.numeric(li5==re5)) %>%
  select(-(li1:li5),-(re1:re5),-(distrt1:distrt5),-dummy, trial)
lsky10resp$acc <- rowMeans(lsky10resp[,9:13])
lsky10resp$rt <- rowMeans(lsky10resp[,4:8])

lsky10resp$cond[lsky10resp$cond == 3] <- 2
lsky10resp$cond[lsky10resp$cond == 13] <- 3

lewandowsky10 <- lsky10resp %>% select(subject, trial, cond, rt, acc)
save(lewandowsky10, file="./pkg/data/lewandowsky10.rda", compress = "xz")

### reproduce Figure 5 (Oberauer et al., 2018)

plotd <- aggregate(acc ~ cond, data = lewandowsky10, FUN = mean)
plot(x=c(0,3), y=c(0.3,1.0), type = "n", xlab = "Condition",
     ylab = "Proportion Correct", main = "Forgetting in Verbal Complex Span",
     xaxt = "n")
axis(side = 1, at = c(0,1,2,3), labels = c("Quiet", "1 distractor", "3 identical distr.", "3 different distr."))
lines(x = plotd$cond, y = plotd$acc, type = "b", lty = 1, pch = 15)

### read in data from Ricker (2014)

fnam <- paste0(pth, "Ricker.2014.txt")
r14 <- read.table(fnam,header=TRUE)
names(r14) <- c("subject","RI","ITI","accuracy","RT","corchar","respchar","setsize","ptrl")

## the following is copied and I don't know yet what it actually means..
#aggregate in steps to compute SE correctly

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

r14m1 <- aggregate(accuracy ~ subject+RI+ITI, data=r14, FUN=mean)
r14m1 <- BakemanL(r14m1, id="subject", dv="accuracy")
r14se <- aggregate(accuracy ~ RI+ITI, data=r14m1,FUN=function(x) 1.96*sd(x)/sqrt(length(x)))
r14m <- aggregate(accuracy ~ RI+ITI, data=r14m1, FUN=mean)
r14m <- merge(r14m,rename(r14se,se=accuracy))
r14$RI <- (r14$RI+10)/1000  #add the missing 10 ms as per Ricker email and then convert to seconds
######################

r14$ITI <- as.character(r14$ITI)

r14$ITI[r14$ITI == "short"] <- "1"
r14$ITI[r14$ITI == "medium"] <- "6"
r14$ITI[r14$ITI == "long"] <- "12"

r14$ITI <- as.numeric(r14$ITI)

r14 <- r14 %>% rename(corr_resp = corchar) %>% rename(resp = respchar)

ricker14 <- r14
save(ricker14, file="./pkg/data/ricker14.rda", compress = "xz")

pd <- aggregate(accuracy ~ RI*ITI, data = ricker14, FUN = mean)
plot(c(0.0,12), c(0.6,0.9), type = "n", xlab = "Retention Interval (s)",
     ylab = "Proportion correct", main = "Figure 2 in Ricker et al. (2014)", xaxt = "n")
axis(side = 1, at = c(1,6,12), labels = T)
lines(x = pd$RI[pd$ITI == 1], y = pd$accuracy[pd$ITI == 1],
      type = "b", lty = 1, pch = 15)
lines(x = pd$RI[pd$ITI == 6], y = pd$accuracy[pd$ITI == 6],
      type = "b", lty = 2, pch = 16)
lines(x = pd$RI[pd$ITI == 12], y = pd$accuracy[pd$ITI == 12],
      type = "b", lty = 3, pch = 17)
legend(12, 0.9, c("1s", "6s", "12s"), lty = 1:3, pch=15:17,
       title = "Inter-trial Interval (s):",
       horiz = T, cex = 0.6, yjust = 1, xjust = 1)




