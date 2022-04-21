## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM3.3 #####################
pth  <- "BenchmarksWM.Data/BM3.3.SP.Latency/"

### Farrell & Lewandowsky (2004). It is yet unclear to me, to which Benchmark this belongs.

fnam <- paste0(pth, "Farrell.Lewandowsky.2004.Exp2.dat")
fl04 <- read.table(fnam, header=FALSE)  # Data from Farrell  & Lewandowsky, 2004, Exp. 2
names(fl04) <- c("subject", "trial", "condition",paste("opp",c(1:6),sep=""),paste("rt",c(1:6),sep=""))
#The columns are subject, trial, condition  (0=no  interference, 1 = interference),
#the recall order (columns are output positions), and the recall latencies
#intrusions prevented, omissions coded as -9

fl04rt   <- fl04 %>% gather("serpos","rt",rt1:rt6) %>%
  select(subject,trial,condition,serpos,rt) %>%
  mutate(serpos=as.numeric(substr(serpos,3,10)))
fl04pos <- fl04 %>% gather("opp","outpos", opp1:opp6) %>%
  select(subject,trial,condition,opp,outpos) %>%
  mutate(opp=as.numeric(substr(opp,4,10)))

fl04tot <- cbind(fl04rt, fl04pos$outpos)
names(fl04tot)[which(names(fl04tot) == "fl04pos$outpos")] <- "outpos"

fl04tot$outpos[fl04tot$outpos < 0] <- NA

## all data:

farrell04 <- fl04tot %>% select(subject,trial,condition,serpos,outpos,rt)
save(farrell04, file="./pkg/data/farrell04.rda")


source("BenchmarksWM.Data/Functions/BakemanL.R")
source("BenchmarksWM.Data/Functions/Confint.R")


fl04rtagg <- aggregate(rt ~ subject + condition + serpos, data=farrell04, FUN=mean)
fl04RT <- BakemanL(fl04rtagg, id="subject", dv="rt")

fl04m     <- aggregate(rt~condition+serpos, data=fl04rtagg, FUN=mean)
fl04SE    <- aggregate(rt~condition+serpos, data=fl04rtagg, FUN=function(x) sd(x)/sqrt(length(x))) #SE for within-subjects comparisons

#now do some plotting
par(mfrow=c(1,1))  #accuracy and then RT
bgk <- c("gray","black")
ltyk <- c("solid","dashed")

#now plot RT by serial position
plot(c(0,7),c(0,3000), xlim=c(0.5,6.5),ylim=c(0,3100), type="n", las=1,
     xlab="Serial Position", ylab="Mean Latency (ms)",cex.lab=1.2,cex.axis=1.)
for (k in c(0:1)) {
  tbp <- filter(fl04m,condition==k)
  xx <- tbp$serpos - 0.05 + k*0.1
  lines(xx,tbp$rt,lwd=2,lty=ltyk[k+1])
  #tbpSE <- filter(fl04SE,condition==k)
  #arrows(xx,tbp$rt-1.96*tbpSE$rt, xx, tbp$rt+1.96*tbpSE$rt, length=0.05, angle=90, code=3)
  points(xx,tbp$rt,pch=21+k,bg=bgk[k+1],cex=1.5)
}
legend(6,2900,c("No interference","Interference"),lty=ltyk,pch=20+c(1:2),pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=1)


########################## Read Murdock & Okada (1970) data: Inter-Response Times for Free Recall

fnam <- paste0(pth, "MurdockOkada.1970.txt")

mo70 <- read.table(fnam, header=F)
names(mo70) <- c("RunningCount", "ID", "trial",
                 "inpos1", "inpos2", "inpos3","inpos4", "inpos5", "inpos6","inpos7", "inpos8", "inpos9","inpos10", "inpos11", "inpos12","inpos13",
                 "time1", "time2", "time3","time4", "time5", "time6","time7", "time8", "time9","time10", "time11", "time12","time13")
# inpos is the input serial position recalled, ordered by output position (according to Kahana, code 88 signals an extralist intrusion, but that is probably 99)


### let's gather this data (It's BM 3.4.2..)

moip <- gather(data = mo70, key = "output_bad", value = "input", 4:16)

moip <- moip %>% select(RunningCount, ID, trial, output_bad, input)
moip$output <- 0
for (i in rev(seq(1:13))) {
  ind <- as.character(i)
  selOut <- grepl(ind, moip$output_bad, fixed = TRUE)
  moip$output[selOut] <- i
  moip$output_bad[selOut] <- NA
}

moirl <- gather(data = mo70, key = "output_bad", value = "irl", 17:29)

moirl <- moirl %>% select(RunningCount, ID, trial, output_bad, irl)
moirl$output <- 0
for (i in rev(seq(1:13))) {
  ind <- as.character(i)
  selOut <- grepl(ind, moirl$output_bad, fixed = TRUE)
  moirl$output[selOut] <- i
  moirl$output_bad[selOut] <- NA
}

# check for same order
moip$run_trial_output <- as.numeric(moip$RunningCount)*10000 + as.numeric(moip$trial)*100 + moip$output
moirl$run_trial_output_irl <- as.numeric(moirl$RunningCount)*10000 + as.numeric(moirl$trial)*100 + moirl$output


molong <- cbind(moip, moirl$run_trial_output_irl, moirl$irl)
names(molong)[which(names(molong) == "moirl$run_trial_output_irl")] <- "rtoirl"
names(molong)[which(names(molong) == "moirl$irl")] <- "irl"

check <- sum(abs(molong$run_trial_output - molong$rtoirl))
#it works

molong <- molong %>% select(ID, trial, output, input, irl)
molong$correct <- 0
molong$correct[molong$input != 0 & molong$input != 99] <- 1

molong$irl[molong$irl < 0.1] <- NA
molong$irl[molong$input == 0] <- NA

murdock70 <- molong %>% rename(subj = ID) %>%
  filter(input > 0) %>%
  group_by(subj, trial) %>%
  mutate(
    num_correct = sum(correct)
  )


save(murdock70, file="./pkg/data/murdock70.rda")
## a plot:


pd <- aggregate(correct ~ input, data = murdock70, FUN = function(x){sum(x)})
pd <- pd[which(pd$input != 0 & pd$input != 99), ]
plot(c(0,20),c(0,1300), xlim=c(1,20),ylim=c(0,1300), type="n", las=1,
     main = "Serial Position Curve by Frequency",
     xlab="Serial Position", ylab="Frequency of recall",cex.lab=1.2,cex.axis=1.)
lines(x = pd$input,
      y = pd$correct,
      type = "l", lty = 1)

## in the original, inter-response latencies are plotted
## dependent on correct recall.
## here's an approximation
## from the plot it seems that extra-list items were excluded

prep_pd <- murdock70 %>% filter(num_correct %in% 4:9) %>%
  group_by(subj, trial) %>%
  mutate(
    legal = case_when(max(output) > num_correct ~ 0,
                      TRUE ~ 1)
  )

next_prep <- prep_pd %>% filter(legal == 1) %>% filter(output > 1)

pdirl <- aggregate(irl ~ output + num_correct, data = next_prep, FUN = mean)
plot(c(1,9),c(0,12), xlim=c(1,9),ylim=c(0,13), type="n", las=1,
     main = "IRL partioned on total number of words recalled",
     xlab="Output Position", ylab="IRL (seconds)",cex.lab=1.2,cex.axis=1.)
## create lines with a for-loop
for (op in sort(unique(pdirl$num_correct))) {
  lines(x = pdirl$output[pdirl$num_correct == op],
        y = pdirl$irl[pdirl$num_correct == op],
        type = "l", lty = op-3)
  points(x = pdirl$output[pdirl$num_correct == op],
         y = pdirl$irl[pdirl$num_correct == op],
         pch = op+12, bg = "black")
}
legend(1,12,sort(unique(pdirl$num_correct)),lty=sort(unique(pdirl$num_correct))-3,pch=sort(unique(pdirl$num_correct))+12,pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=0)


## Oeztekin & McElree (2010) (BM3.3.2 Fast Access to Last Item)
pth <- "BenchmarksWM.Data/BM3.3.2.SAT.LastItem/"
for (id in 1:19) {
  datafile <- paste(pth, "/Oeztekin.McElree.2010/S", as.character(id), ".dat", sep="")
  dat <- read.table(datafile, header=F)
  dat <- cbind(rep(id, dim(dat)[1]), dat)
  if (id == 1) SATdat <- dat
  if (id > 1) SATdat <- rbind(SATdat, dat)
}
SATdat <- SATdat[,c(1:4, 15:19)]
names(SATdat) <- c("id", "trial", "cond", "lag", "flag", "lagrt", "rt", "corr", "confidence")

SATdat <- SATdat[SATdat$flag==0 & SATdat$rt < 600,]  # eliminate not-to-be-used trials (according to Ilke Oeztekin)

SATdat$serpos[is.element(SATdat$cond, c(0,1))] <- 1
SATdat$serpos[is.element(SATdat$cond, c(2,3))] <- 2
SATdat$serpos[is.element(SATdat$cond, c(4,5))] <- 3
SATdat$serpos[is.element(SATdat$cond, c(6,7))] <- 4
SATdat$serpos[is.element(SATdat$cond, c(8,9))] <- 5
SATdat$serpos[is.element(SATdat$cond, c(10,11))] <- 6
SATdat$ptype[is.element(SATdat$cond, 1:11)] <- 1  # all positive probes
SATdat$ptype[is.element(SATdat$cond, c(12:14))] <- 3  # all lures from 1st category
SATdat$ptype[is.element(SATdat$cond, c(15:17))] <- 4  # all lures from 2nd category
SATdat$ptype[is.element(SATdat$cond, c(18:20))] <- 5  # recent negative lure from previous trial
SATdat$ptype[is.element(SATdat$cond, c(21:23))] <- 2  # new lure

oeztekin10 <- SATdat %>% select(id, trial, lag, serpos, ptype, lagrt, rt, corr)
save(oeztekin10, file="./pkg/data/oeztekin10.rda")

SATpos <- subset(oeztekin10, ptype==1)

## Plot
bgk <- c("black", "grey20", "grey40", "grey60", "grey80", "white")

maxx <- 3.5
miny <- 0.4
plot(c(0,3.5),c(0,1), xlim=c(0, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Lag + RT (s)", ylab="Proportion Correct",cex.lab=1, cex.axis=1)
for (sp in 1:6) {
  sat <- subset(SATpos, serpos==sp)
  pd <- aggregate(corr ~ lag, data=sat, FUN=mean)
  pd_rt <- aggregate(lagrt ~ lag, data = sat, FUN = mean)
  pd_whole <- cbind(pd, pd_rt$lagrt)
  pd_whole$lagrt <- pd_whole$`pd_rt$lagrt`/1000
  lines(x = pd_whole$lagrt,
        y = pd_whole$corr,
        type = "l", lty = 2)
  points(x = pd_whole$lagrt,
         y = pd_whole$corr,
         pch = 21, bg = bgk[sp])
}
legend(maxx,miny,c("SP=1", "SP=2", "SP=3", "SP=4", "SP=5", "SP=6"), pch=21, pt.bg = bgk, xjust=1, yjust=0)

