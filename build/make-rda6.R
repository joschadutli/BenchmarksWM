## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM6 Auditory Distraction #####################
pth  <- "BenchmarksWM.Data/BM6.IrrelevantSound/"

### Schlittmeier et al (2012)

irrspeech <- read_excel(paste0(pth,"Vp-Daten SJS-Diss Sprache.xls"),sheet=4,col_names=T)  # sheet 4: visual presentation, IS throughout
irrmusic <- read_excel(paste0(pth,"Vp-Daten SJS-Diss Musik.xls"),sheet=3,col_names=T)     # sheet 3: visual presentation, IS throughout

# subtract irrspeech from 100 because data are error percentages!
changing <- (100-irrspeech[,which(names(irrspeech)=="item1ch.st."):which(names(irrspeech)=="item9ch.st.")])/100
steady <- (100-irrspeech[,which(names(irrspeech)=="item1st.st."):which(names(irrspeech)=="item9st.st.")])/100
quiet <- (100-irrspeech[,which(names(irrspeech)=="item1ruhe"):which(names(irrspeech)=="item9ruhe")])/100

stakkato <- (100-irrmusic[,which(names(irrmusic)=="pos1stakkato"):which(names(irrmusic)=="pos9stakkato")])/100
legato <- (100-irrmusic[,which(names(irrmusic)=="pos1legato"):which(names(irrmusic)=="pos9legato")])/100
mquiet <- (100-irrmusic[,which(names(irrmusic)=="pos1ruhe"):which(names(irrmusic)=="pos9ruhe")])/100

changing <- cbind(irrspeech$Vp, changing)
steady <- cbind(irrspeech$Vp, steady)
quiet <- cbind(irrspeech$Vp, quiet)

stakkato <- cbind(irrmusic$Vp, stakkato)
legato <- cbind(irrmusic$Vp, legato)
mquiet <- cbind(irrmusic$Vp, mquiet)

changing$condition <- "changing"
steady$condition <- "steady"
quiet$condition <- "quiet"

stakkato$condition <- "staccato"
legato$condition <- "legato"
mquiet$condition <- "quiet"

change <- gather(changing, key = "serpos", value = "acc", 2:10)
stead <- gather(steady, key = "serpos", value = "acc", 2:10)
qui <- gather(quiet, key = "serpos", value = "acc", 2:10)

stac <- gather(stakkato, key = "serpos", value = "acc", 2:10)
leg <- gather(legato, key = "serpos", value = "acc", 2:10)
mqui <- gather(mquiet, key = "serpos", value = "acc", 2:10)

sound <- rbind(change,stead,qui)
music <- rbind(stac,leg,mqui)

sound$type <- "speech"
music$type <- "music"

sound <- sound %>% rename(subj = `irrspeech$Vp`)
music <- music %>% rename(subj = `irrmusic$Vp`)

sound$serpos <- substr(sound$serpos,5,5)
sound$serpos <- as.numeric(sound$serpos)

music$serpos <- substr(music$serpos,4,4)
music$serpos <- as.numeric(music$serpos)

schlitt <- rbind(sound,music)

schlittmeier12 <- schlitt %>% select(subj, type, condition, serpos, acc)

save(schlittmeier12, file = "./pkg/data/schlittmeier12.rda")

### Figure 15 in Oberauer et al. (2018)

pdd <- aggregate(acc ~ serpos+type+condition, data = schlittmeier12, FUN = mean)
pd <- subset(pdd, type == "speech")
plot(c(0,10), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Irrelevant speech and sound effects", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = levels(pd$condition), cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "quiet"], y = pd$acc[pd$condition == "quiet"], 
      type = "b", lty = 1, pch = 15, col = "red")
lines(x = pd$serpos[pd$condition == "steady"], y = pd$acc[pd$condition == "steady"], 
      type = "b", lty = 2, pch = 17, col = "blue")
lines(x = pd$serpos[pd$condition == "changing"], y = pd$acc[pd$condition == "changing"], 
      type = "b", lty = 3, pch = 18, col = "green")
legend(0, 0, c("quiet", "steady", "changing"), lty = 1:3, pch=c(15,17, 18), 
       title = "Condition:",
       col = c("red", "blue", "green"), horiz = F, cex = 0.6, yjust = 0, xjust = 0)

##########################################
### Bell et al. (2019)

rm(list=ls())
pth  <- "BenchmarksWM.Data/BM6.IrrelevantSound/Raoul.Bell.Replication/"


data <- read.table(paste0(pth,"BENCHMARK_RAW_DATA.txt"), header=F)
names(data) <- c("id", "session", "trial", "condition", 
                 "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                 "REM",
                 "resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7", "resp8",
                 "SCORE", "Numremembered", "rt", 
                 "distr1", "distr2", "distr3", "distr4", "distr5", "distr6", "distr7", "distr8", "distr9", "distr10")
# conditions: SS = steady-state; CS = changing-state; DEV = auditory deviant

data$condnum <- 1
data$condnum[data$condition=="CS"] <- 2
data$condnum[data$condition=="DEV"] <- 3
data$corr1 <- as.numeric(data$stim1 == data$resp1)
data$corr2 <- as.numeric(data$stim2 == data$resp2)
data$corr3 <- as.numeric(data$stim3 == data$resp3)
data$corr4 <- as.numeric(data$stim4 == data$resp4)
data$corr5 <- as.numeric(data$stim5 == data$resp5)
data$corr6 <- as.numeric(data$stim6 == data$resp6)
data$corr7 <- as.numeric(data$stim7 == data$resp7)
data$corr8 <- as.numeric(data$stim8 == data$resp8)

# turn into long format
datalong <- data %>% gather("serpos", "correct", corr1, corr2, corr3, corr4, corr5, corr6, corr7, corr8)
datalong <- datalong[, c("id", "session", "trial", "condnum", "serpos", "correct")]

datalong$serpos <- sapply( strsplit(datalong$serpos , split="rr"), function(x) (as.numeric(x[2])) )

datalong$condition[datalong$condnum == 1] <- "steady"
datalong$condition[datalong$condnum == 2] <- "changing"
datalong$condition[datalong$condnum == 3] <- "auditory deviant"

bell19 <- datalong %>% select(id, session, trial, condition, serpos, correct)
bell19 <- bell19 %>% rename(subj = id)
bell19$session <- as.character(bell19$session)
bell19$session[bell19$session == "Day1"] <- 1
bell19$session[bell19$session == "Day2"] <- 2

save(bell19, file = "./pkg/data/bell19.rda")

pd <- aggregate(correct ~ serpos+condition, data = bell19, FUN = mean)
plot(c(1,8), c(0.3,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Auditory Deviant Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "steady"], y = pd$correct[pd$condition == "steady"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$condition == "auditory deviant"], 
      y = pd$correct[pd$condition == "auditory deviant"], 
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$condition == "changing"], y = pd$correct[pd$condition == "changing"], 
      type = "b", lty = 3, pch = 18, col = "green")
legend(1, 0.3, c("steady", "auditory deviant", "changing"), lty = 1:3, pch=c(15,17, 18), 
       title = "Condition:",
       col = c("blue", "red", "green"), horiz = F, cex = 0.6, yjust = 0, xjust = 0)

