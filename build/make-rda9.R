## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM9 Distinctiveness and Grouping #####################
pth  <- "BenchmarksWM.Data/BM9.1.TempIsolation/"

NimmoLsky <- read.table(paste0(pth, "AudiVisData.txt"),
                        col.names=c("subject", "session", "condition",
                                    "item1", "item2", "item3", "item4", "item5", "item6", "item7",
                                    "iti1", "iti2", "iti3", "iti4", "iti5", "iti6",
                                    "resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7",
                                    "rt1", "rt2", "rt3", "rt4", "rt5", "rt6", "rt7"))
# condition: v = visual, a = auditory
# item1 to item7: list items in their order of presentation
# iti1 to iti6: inter-item intervals 1 to 6 in seconds
# resp1 to resp7: recalled items (i.e., the key that was pressed)
# rt1 to rt7: response times in seconds

trial <- 1:360
subj_vec <- sort(unique(NimmoLsky$subject))
nimlew <- NULL
for (ss in subj_vec) {
  d <- subset(NimmoLsky, subject == ss)
  dd <- cbind(d, trial)
  nimlew <- rbind(nimlew, dd)
}

nimlew$totaltime <- rowSums(nimlew[,11:16])

nimlew$iti7 <- 0

presd <- gather(nimlew, key = "serpos", value = "item", item1:item7)
presd <- presd %>% select(subject, session, trial, condition, serpos, item)

timed <- gather(nimlew, key = "serpos", value = "iti", c(iti1:iti6,iti7))
timed <- timed %>% select(subject, session, trial, condition, serpos, iti)

respd <- gather(nimlew, key = "serpos", value = "response", resp1:resp7)
respd <- respd %>% select(subject, session, trial, condition, serpos, response)

rtd <- gather(nimlew, key = "serpos", value = "rt", rt1:rt7)
rtd <- rtd %>% select(subject, session, trial, condition, serpos, rt)

lew08 <- cbind(presd, timed$iti, respd$response, rtd$rt)
lew08 <- lew08 %>% dplyr::rename(subj = subject, iti = `timed$iti`,
                          response = `respd$response`, rt = `rtd$rt`)

lew08$condition <- as.character(lew08$condition)
lew08$condition[lew08$condition == "v"] <- "visual"
lew08$condition[lew08$condition == "a"] <- "auditory"

lew08$serpos <- as.numeric(substr(lew08$serpos,5,5))

lew08$correct <- as.numeric(lew08$item==lew08$response)

nimmo06 <- lew08
nimmo06$response <- as.character(nimmo06$response)
nimmo06 <- nimmo06 %>% dplyr::rename(postitem.interval = iti)
save(nimmo06, file = "./pkg/data/nimmo06.rda", compress = "xz")

pd <- aggregate(correct ~ serpos+condition, data = nimmo06, FUN = mean)
plot(c(1,7), c(0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct",
     main = "Serial Position Curves", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7), labels = unique(pd$serpos),
     cex.axis = 1.0)
lines(x = pd$serpos[pd$condition == "visual"],
      y = pd$correct[pd$condition == "visual"],
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$serpos[pd$condition == "auditory"],
      y = pd$correct[pd$condition == "auditory"],
      type = "b", lty = 2, pch = 16, col = "darkgrey")
legend(1, 0, c("visual", "auditory"),
       lty = 1:2,
       pch=15:16, col = c("black","darkgrey"),
       horiz = F, cex = 0.9, yjust = 0, xjust = 0)


### Morin

# Recognition data (Experiment 1) of Morin, Brown, & Lewandowsky (2010).
rm(list = ls())
pth  <- "BenchmarksWM.Data/BM9.1.TempIsolation/"
MorinRecog <- read.table(paste0(pth,"data recognition random.txt"), header=T)

# gap1 to gap8: iter-item intervals in order of presentation: number of digits to be shadowed in the gap (each digit = 550 ms)
# position: serial position tested (for positive probes; new probes are not included in the data set)
# CorrectR: letters that code a correct response (ur = u or r are correct)
# Response: p = new, r = remember, u = know (??)
# Type: Yes = probe was included in the list
# pre/ post: number of digits preceding / following the position of the item matching the probe
# TotalIsolation = sum of pre and post
# TotalTime = retention interval = total time between presentation of the item matching the probe and presentation of the probe

morin10 <- MorinRecog %>% dplyr::rename(subj = Subject, age = Age, sex = Sex, trial = Trial,
                                 serpos = position, acc = Accuracy, rt = RT,
                                 probe = Type, resp = Resp, isolation = TotalIsolation,
                                 totaltime = TotalTime)
morin10$iso.time <- morin10$isolation*550
morin10 <- morin10 %>% select(subj,age,sex,trial,totaltime,isolation,iso.time,serpos,probe,
                              resp,acc,rt)

morin10$probe <- as.character(morin10$probe)
morin10$probe[is.na(morin10$probe)] <- "positive"

bins.prep <- sort(unique(morin10$iso.time))
bins.values <- c(0, rep(sum(bins.prep[2:4])/3,3), rep(sum(bins.prep[5:6])/2,2),
                 rep(sum(bins.prep[7:8])/2,2), rep(sum(bins.prep[9:10])/2,2),
                 rep(sum(bins.prep[11:14])/4,4))
morin10$bin[morin10$iso.time == 0] <- 0
for (j in 2:14) {
  morin10$bin[morin10$iso.time == bins.prep[j]] <- bins.values[j]
}

morin10 <- morin10 %>% select(subj, age, sex, trial, totaltime, iso.time, bin, serpos,
                              probe, resp, acc, rt)

save(morin10, file = "./pkg/data/morin10.rda", compress = "xz")

pd <- aggregate(acc ~ bin+resp, data = morin10, FUN = sum)

pd <- spread(morin10, key = "resp", value = "acc")

pd$score <- pd$r/pd$u
pd$diff <- pd$u/pd$r


# pdd <- plyr::count(morin10, vars = c("resp","bin","subj"))
#
# pdd <- spread(pd, key = "resp", value = "freq")
#
# pdd <- pd[2:6,]

# pdd$r[is.na(pdd$r)] <- 0
# pdd$u[is.na(pdd$u)] <- 0
# pdd$p[is.na(pdd$p)] <- 0
# pd$tot <- pd$r+pd$u+pd$p

pdd <- pd
pdd$score <- pdd$r/pdd$tot
pdd$uscore <- pdd$u/pdd$tot

pdd$bin <- pdd$bin/1000

# pddd <- aggregate(score ~ bin, data = pdd, FUN = mean)
# pdddd <- aggregate(uscore ~ bin, data = pdd, FUN = mean)
#
# ### plot this data
#
# pddd$bin <- pddd$bin/1000
# pddd <- pddd[2:6,]
# pdddd$bin <- pdddd$bin/1000
# pdddd <- pdddd[2:6,]

twoseven <- morin10[which(morin10$serpos %in% c(2,3,4,5,6,7)),]

pd <- plyr::count(twoseven, vars = c("resp","bin"))
pd <- tidyr::spread(pd, key = "resp", value = "freq")

pd$tot <- pd$r+pd$u+pd$p
pd$score <- pd$r/pd$tot
pd$uscore <- pd$u/pd$tot
pd$bin <- pd$bin/1000

plot(c(0,10), c(0,0.8), type = "n", xlab = "Combined Temporal Isolation (sec)",
     ylab = "Proportion correct",
     main = "Figure 2 in Morin et al. (2010)", xaxt = "n")
axis(side = 1, at = c(0,2,4,6,8,10), labels = c(0,2,4,6,8,10),
     cex.axis = 1.0)
points(x = pd$bin, y = pd$score,
       type = "p", lty = 1, pch = 15)
abline(lm(score ~ bin, data = pd), lty = 1, pch = 15)
points(x = pd$bin, y = pd$uscore,
       type = "p", lty = 2, pch = 17)
abline(lm(uscore ~ bin, data = pd), lty = 2, pch = 17)
legend(10, 0, c("remember", "know"),
       lty = 1:2,
       pch=c(15,17),
       horiz = F, cex = 0.9, yjust = 0, xjust = 1)




####################################################
### Grouping (9.2.3)

rm(list=ls())
############### BM9 Distinctiveness and Grouping #####################
pth  <- "BenchmarksWM.Data/BM9.2.Grouping/"

#function to rearrange data into column format
reformatdata <- function(aud) {
  aud2 <- aud %>% gather("subject","listrec",s01:s16) %>%
    #  select(subject,list,listrec,cond,trial) %>%
    mutate(listrec=as.character(listrec),list=as.character(list)) %>%
    separate(list,paste("li",c(1:9),sep=""),sep=c(1:8),convert=TRUE) %>%
    separate(listrec,paste("r",c(1:9),sep=""),sep=c(1:8),convert=TRUE)

  audlist <- aud2 %>% gather("spos","item",li1:li9) %>% select(-(r1:r9)) %>%
    mutate(spos=as.numeric(substr(spos,3,10))) %>% arrange(subject,trial,spos)
  audresp <- aud2 %>% gather("spos","resp",r1:r9) %>% select(-(li1:li9)) %>%
    mutate(spos=as.numeric(substr(spos,2,10))) %>% arrange(subject,trial,spos)
  return(inner_join(audlist,audresp,by=c("subject","trial","cond","spos")))
}

#read data
aud <- read_excel(paste0(pth,"Frankish.1989.Exp1234.xlsx"),sheet="Exp 1 auditory", col_names=TRUE,skip=2)
aud <- aud[1:60,1:19]
vis <- read_excel(paste0(pth,"Frankish.1989.Exp1234.xlsx"),sheet="Exp 1 visual",col_names=TRUE,skip=2)
vis <- vis[1:60,1:19]
#reshape the data into the required column format
audfin <- reformatdata(aud)
visfin <- reformatdata(vis)

audfin$acc <- as.numeric(audfin$resp==audfin$item)
visfin$acc <- as.numeric(visfin$resp==visfin$item)

# identify conditions
chechd <- aggregate(acc ~ cond, data = visfin[which(visfin$spos == 6),], FUN = mean)
chechd$err <- 1-chechd$acc

## --> conditions: 1 = ungrouped, 2 = 0.25 s gap, 3 = 0.5 s gap, 4 = 1 s gap, 5 = 2 s gap

## subj ids

audfin$subject <- as.numeric(substr(audfin$subject,2,3))
visfin$subject <- as.numeric(substr(visfin$subject,2,3))
visfin$trial <- as.numeric(visfin$trial)
audfin$trial <- as.numeric(audfin$trial)

visfin$group <- "visual"
audfin$group <- "auditory"
visfin$subject <- 16+visfin$subject

frankish89 <- rbind(audfin, visfin)

frankish89$pause[frankish89$cond == 1] <- 0
frankish89$pause[frankish89$cond == 2] <- 0.25
frankish89$pause[frankish89$cond == 3] <- 0.5
frankish89$pause[frankish89$cond == 4] <- 1.0
frankish89$pause[frankish89$cond == 5] <- 2.0
frankish89$condition <- "grouped"
frankish89$condition[frankish89$cond == 1] <- "ungrouped"

## tidy up

frankish89 <- frankish89 %>% dplyr::rename(cond_num = cond, subj = subject, serpos = spos,
                                    interval = pause)
frankish89 <- frankish89 %>% select(subj, trial, group, condition, cond_num, interval,
                                    serpos, item, resp, acc)

save(frankish89, file="./pkg/data/frankish89.rda", compress = "xz")

## Figure 18 in Oberauer et al. (2018)

subdat <- subset(frankish89, group == "visual")
pd <- aggregate(acc ~ serpos+cond_num, data = subdat, FUN = mean)
bgk <- c("gray","black","gray","black","white")
plot(0,0, xlim=c(0.5,9.5),ylim=c(0.2,1), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1.4,cex.axis=1.2)
title("Visual")
axis(1, at=c(1:9),lwd=1, lwd.ticks=1,cex.axis=1.0)
axis(2, at=seq(from=.3,to=1.,by=.1),lwd=1, lwd.ticks=1,cex.axis=1.0)
for (k in c(1:5)) {
  tbp <- pd[which(pd$cond_num == k),]
  lines(tbp$serpos[1:3],tbp$acc[1:3],lwd=1,lty=k)
  lines(tbp$serpos[4:6],tbp$acc[4:6],lwd=1,lty=k)
  lines(tbp$serpos[7:9],tbp$acc[7:9],lwd=1,lty=k)
  points(tbp$serpos,tbp$acc,pch=20+k,bg=bgk[k],cex=1.0)
}

legend(1,.5,c("Ungrouped","0.25 s", "0.5 s", "1 s", "2 s"),lty=c(1:5),pch=20+c(1:5),pt.bg=bgk,cex=0.6,pt.cex=1.0)



##################################################
##### farrell09 and lelièvre
rm(list = ls())
pth  <- "BenchmarksWM.Data/BM9.2.3 Grouping Interpositions/"

### read farrell09a i.e., exp1

exp1 <- read.table(paste0(pth, "group44.dat"), header = F)
names(exp1) <- c("subj", "trial", "session", "item1", "item2",
                 "item3", "item4", "item5",
                  "item6", "item7", "item8", "rt1", "rt2", "rt3", "rt4",
                 "rt5", "rt6", "rt7", "rt8")

exp1$dist.pc <- NA
exp1$dist.rt <- NA


acc1 <- gather(exp1, key = "serpos", value = "outpos", 4:11)
acc1$grouping[acc1$session == 0] <- "8"
acc1$grouping[acc1$session == 1] <- "4-4"
acc1 <- acc1 %>% select(subj, trial, grouping, dist.pc, dist.rt, serpos, outpos)


rt1 <- gather(exp1, key = "serpos", value = "rt", 12:19)

farrell09a <- cbind(acc1, rt1$rt)

farrell09a$serpos <- as.numeric(substr(farrell09a$serpos,5,5))
farrell09a <- farrell09a %>% dplyr::rename(rt = `rt1$rt`)

farrell09a$acc <- as.numeric(farrell09a$serpos==farrell09a$outpos)

farrell09a$exp <- 1

farrell09a <- farrell09a %>% select(exp, subj, trial, grouping,
                                    dist.pc, dist.rt, serpos,
                                    outpos, acc, rt)

### exp2 4-4

exp2a <- read.table(paste0(pth, "grouplong44.dat"), header = F)
names(exp2a) <- c("subj", "item1", "item2", "item3", "item4", "item5",
                  "item6", "item7", "item8", "rt1", "rt2",
                  "rt3", "rt4", "rt5", "rt6", "rt7", "rt8",
                  "dist1", "dist2", "dist3", "dist4",
                  "rt_dist1", "rt_dist2", "rt_dist3", "rt_dist4")
trial <- 1:200
subj_vec <- sort(unique(exp2a$subj))
nimlew <- NULL
for (ss in subj_vec) {
  d <- subset(exp2a, subj == ss)
  dd <- cbind(d, trial)
  nimlew <- rbind(nimlew, dd)
}

## compute by trial means for dists...
nimlew$dist.pc <- rowMeans(nimlew[,18:21], na.rm = T)
nimlew$dist.rt <- rowMeans(nimlew[,22:25], na.rm = T)

acc1 <- gather(nimlew, key = "serpos", value = "outpos", 2:9)
acc1 <- acc1 %>% select(subj, trial, dist.pc, dist.rt, serpos, outpos)


rt1 <- gather(nimlew, key = "serpos", value = "rt", 10:17)

farrell09b <- cbind(acc1, rt1$rt)

farrell09b$serpos <- as.numeric(substr(farrell09b$serpos,5,5))
farrell09b <- farrell09b %>% dplyr::rename(rt = `rt1$rt`)

farrell09b$acc <- as.numeric(farrell09b$serpos==farrell09b$outpos)

## add grouping var and exp

farrell09b$exp <- 2
farrell09b$grouping <- "4-4"

farrell09b <- farrell09b %>% select(exp, subj, trial, grouping,
                                    dist.pc, dist.rt, serpos,
                                    outpos, acc, rt)

# unique(check_b == farrell09b)

### exp2 grouping 3-4
# check_c <- check_farrell09[which(check_farrell09$exp == 2 & check_farrell09$grouping == "3-4"),]

exp2c <- read.table(paste0(pth, "grouplong34.dat"), header = F)
names(exp2c) <- c("subj", "item1", "item2", "item3", "item4", "item5",
                  "item6", "item7", "rt1", "rt2", "rt3", "rt4",
                  "rt5", "rt6", "rt7", "dist1", "dist2",
                  "dist3", "dist4", "rt_dist1", "rt_dist2",
                  "rt_dist3", "rt_dist4")
trial <- 1:100
subj_vec <- sort(unique(exp2c$subj))
nimlew <- NULL
for (ss in subj_vec) {
  d <- subset(exp2c, subj == ss)
  dd <- cbind(d, trial)
  nimlew <- rbind(nimlew, dd)
}

## compute by trial means for dists...
nimlew$dist.pc <- rowMeans(nimlew[,16:19], na.rm = T)
nimlew$dist.rt <- rowMeans(nimlew[,20:23], na.rm = T)

acc1 <- gather(nimlew, key = "serpos", value = "outpos", 2:8)
acc1 <- acc1 %>% select(subj, trial, dist.pc, dist.rt, serpos, outpos)


rt1 <- gather(nimlew, key = "serpos", value = "rt", 9:15)

farrell09c <- cbind(acc1, rt1$rt)

farrell09c$serpos <- as.numeric(substr(farrell09c$serpos,5,5))
farrell09c <- farrell09c %>% dplyr::rename(rt = `rt1$rt`)

farrell09c$acc <- as.numeric(farrell09c$serpos==farrell09c$outpos)

## add grouping var and exp

farrell09c$exp <- 2
farrell09c$grouping <- "3-4"

farrell09c <- farrell09c %>% select(exp, subj, trial, grouping,
                                    dist.pc, dist.rt, serpos,
                                    outpos, acc, rt)

# unique(check_c == farrell09c)

## exp2 and grouping "4-3"
# check_d <- check_farrell09[which(check_farrell09$grouping == "4-3"),]

exp2b <- read.table(paste0(pth, "grouplong43.dat"), header = F)
names(exp2b) <- c("subj", "item1", "item2", "item3", "item4", "item5",
                 "item6", "item7", "rt1", "rt2", "rt3", "rt4",
                 "rt5", "rt6", "rt7", "dist1", "dist2",
                 "dist3", "dist4", "rt_dist1", "rt_dist2",
                 "rt_dist3", "rt_dist4")

trial <- 1:100
subj_vec <- sort(unique(exp2b$subj))
nimlew <- NULL
for (ss in subj_vec) {
  d <- subset(exp2b, subj == ss)
  dd <- cbind(d, trial)
  nimlew <- rbind(nimlew, dd)
}

## compute by trial means for dists...
nimlew$dist.pc <- rowMeans(nimlew[,16:19], na.rm = T)
nimlew$dist.rt <- rowMeans(nimlew[,20:23], na.rm = T)

acc1 <- gather(nimlew, key = "serpos", value = "outpos", 2:8)
acc1 <- acc1 %>% select(subj, trial, dist.pc, dist.rt, serpos, outpos)


rt1 <- gather(nimlew, key = "serpos", value = "rt", 9:15)

farrell09d <- cbind(acc1, rt1$rt)

farrell09d$serpos <- as.numeric(substr(farrell09d$serpos,5,5))
farrell09d <- farrell09d %>% dplyr::rename(rt = `rt1$rt`)

farrell09d$acc <- as.numeric(farrell09d$serpos==farrell09d$outpos)

farrell09d$exp <- 2
farrell09d$grouping <- '4-3'

farrell09d <- farrell09d %>% select(exp, subj, trial, grouping,
                                    dist.pc, dist.rt, serpos,
                                    outpos, acc, rt)

### let's take a look
# unique(check_d == farrell09d)

### c = 34

# farrell09a$grouping <- '4-4'
# farrell09a$grouping[farrell09a$session == 0] <- '8'
# farrell09a <- farrell09a %>% select(subj,trial,serpos,outpos,rt,acc,exp,dist.pc,dist.rt,grouping)
# farrell09b$grouping <- '4-4'
# farrell09c$grouping <- '3-4'
# farrell09d$grouping <- '4-3'


farrell09 <- rbind(farrell09a,farrell09b,farrell09c,farrell09d)

unique(check_farrell09 == farrell09)

# farrell09 <- farrell09 %>% select(exp, subj, trial, grouping,
#                                   dist.pc, dist.rt,
#                                   serpos, outpos, acc, rt)
save(farrell09, file = "./pkg/data/farrell09.rda", compress = "xz")

### Figure 8


subdat <- subset(farrell09, exp == 2)
pd <- aggregate(acc ~ grouping+serpos, data = subdat, FUN = mean)
bgk <- c("black","gray","white")
plot(0,0, xlim=c(0.5,8.5),ylim=c(0.4,0.9), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1.4,cex.axis=1.2)
title("Effects of Grouping")
axis(1, at=c(1:8),lwd=1, lwd.ticks=1,cex.axis=1.0)
axis(2, at=seq(from=.4,to=.9,by=.1),lwd=1, lwd.ticks=1,cex.axis=1.0)
ff <- subset(pd, grouping == '4-4')
lines(ff$serpos[1:4],ff$acc[1:4],lwd=1,lty=1)
lines(ff$serpos[5:8],ff$acc[5:8],lwd=1,lty=1)
points(ff$serpos,ff$acc,pch=20+1,bg=bgk[1],cex=1.0)
tf <- subset(pd, grouping == '3-4')
lines(tf$serpos[1:3],tf$acc[1:3],lwd=1,lty=1)
lines(tf$serpos[4:7],tf$acc[4:7],lwd=1,lty=1)
points(tf$serpos,tf$acc,pch=20+3,bg=bgk[3],cex=1.0)
ft <- subset(pd, grouping == '4-3')
lines(ft$serpos[1:4],ft$acc[1:4],lwd=1,lty=1)
lines(ft$serpos[5:7],ft$acc[5:7],lwd=1,lty=1)
points(ft$serpos,ft$acc,pch=20+2,bg=bgk[2],cex=1.0)
legend(1,.55,c("4-4","4-3", "3-4"),pch=20+c(1:3),pt.bg=bgk,cex=0.8,pt.cex=1.0)






