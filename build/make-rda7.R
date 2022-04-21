## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM7 Word Length Effect #####################
pth  <- "BenchmarksWM.Data/BM7.WLE/"

## Bhatarah 2009

id_vec <- 1:24
startin <- 5
trial_vec <- 1:32
num_cells <- 8
num_skip <- 6
num_skip_subj <- 2
data <- NULL
for (subj in id_vec) {
  for (tr in trial_vec) {
    endin <- startin + num_cells
    r <- paste0("A",startin,":O",endin)
    ### read in data
    d <- read_excel(paste0(pth,"BhatarahWardSmith&Hayes(2009).xls"), sheet = "Experiment 1",
                    range = r)
    d <- d[,c(1:5,15)]
    data <- rbind(data,d)
    startin <- startin + num_cells + num_skip
  }
  startin <- startin + num_skip_subj
}

bhat <- data

bhat <- bhat %>% rename(trial = list, serpos = listpos...3, condition = trial, output = ...15)
bhat$recalled <- 0
bhat$recalled[!is.na(bhat$output)] <- 1
bhat$spos.correct <- 0
bhat$spos.correct[bhat$output == bhat$serpos] <- 1
bhat$task[bhat$condition == "SP"] <- "SR"
bhat$task[bhat$condition == "SM"] <- "SR"
bhat$task[bhat$condition == "FP"] <- "FR"
bhat$task[bhat$condition == "FM"] <- "FR"

bhat$cue[bhat$condition == "SP"] <- "precued"
bhat$cue[bhat$condition == "FP"] <- "precued"
bhat$cue[bhat$condition == "SM"] <- "postcued"
bhat$cue[bhat$condition == "FM"] <- "postcued"

bhat$exp <- 1
bhat1 <- bhat %>% select(exp, subj, trial, task, cue, serpos, words, output,
                         recalled, spos.correct)


### Experiment 2

d2 <- read_excel(paste0(pth, "BhatarahWardSmith&Hayes(2009).xls"), sheet = "Experiment 2",
                 range = "A1:N13825")

bhat2 <- d2 %>% rename(subj = Subj, block = Block, trial = List, task = Task,
                       presrate = PresRate, length = `Word Length`, serpos = SP,
                       output = `Output order`, recalled = Correct, rehearsals = `#R's`,
                       last.rehearsal = `Last R`, spos.correct = `Correct ISR`)
bhat2$exp <- 2

bhat2 <- bhat2 %>% select(exp, subj, block, trial, task, presrate, length,
                          serpos, rehearsals, last.rehearsal, output, recalled,
                          spos.correct)

bhatarah09a <- bhat1
save(bhatarah09a, file = "./pkg/data/bhatarah09a.rda")

bhat2$trial <- (bhat2$block-1)*max(bhat2$trial) + bhat2$trial
bhat2 <- bhat2 %>% rename(word.length = length)

bhat2$presrate[bhat2$presrate == "SLOW"] <- "slow"
bhat2$presrate[bhat2$presrate == "FAST"] <- "fast"
bhat2$word.length[bhat2$word.length == "S"] <- "short"
bhat2$word.length[bhat2$word.length == "M"] <- "medium"
bhat2$word.length[bhat2$word.length == "L"] <- "long"

bhatarah09b <- bhat2
save(bhatarah09b, file = "./pkg/data/bhatarah09b.rda")

### Plots 1 and 2
### Free recall
pd <- aggregate(recalled ~ serpos+presrate+word.length,
                data = bhatarah09b[which(bhatarah09b$task == "FR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Free Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short" & pd$presrate == "slow"],
      y = pd$recalled[pd$word.length == "short" & pd$presrate == "slow"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium" & pd$presrate == "slow"],
      y = pd$recalled[pd$word.length == "medium" & pd$presrate == "slow"],
      type = "b", lty = 1, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long" & pd$presrate == "slow"],
      y = pd$recalled[pd$word.length == "long" & pd$presrate == "slow"],
      type = "b", lty = 1, pch = 18, col = "green")
lines(x = pd$serpos[pd$word.length == "short" & pd$presrate == "fast"],
      y = pd$recalled[pd$word.length == "short" & pd$presrate == "fast"],
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium" & pd$presrate == "fast"],
      y = pd$recalled[pd$word.length == "medium" & pd$presrate == "fast"],
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long" & pd$presrate == "fast"],
      y = pd$recalled[pd$word.length == "long" & pd$presrate == "fast"],
      type = "b", lty = 2, pch = 18, col = "green")
legend(8, 0.0, c("slow-short","slow-medium","slow-long",
                 "fast-short","fast-medium","fast-long"), lty = c(1,1,1,2,2,2),
       pch=c(15,17,18,15,17,18), title = "Cue condition:",
       col = c("blue", "red","green","blue", "red","green"), horiz = F, cex = 0.6,
       yjust = 0, xjust = 1)

### Experiment 3!


d3_ifr <- read_excel(paste0(pth, "BhatarahWardSmith&Hayes(2009).xls"),
                 sheet = "Experiment 3-IFR",
                 range = "A1:J4609")
d3_isr <- read_excel(paste0(pth, "BhatarahWardSmith&Hayes(2009).xls"),
                     sheet = "Experiment 3-ISR",
                     range = "A1:J4609")
d3 <- rbind(d3_ifr, d3_isr)

bhat3 <- d3 %>% rename(activity = Activity, word.length = wordlength, serpos = SP,
                       output = OutptOrdr, recalled = Correct, spos.correct = ISRCorrect)


bhat3 <- bhat3 %>% rename(trial = list)

bhat3$activity[bhat3$activity == "ARTIC_SUPP"] <- "articulatory suppression"
bhat3$activity[bhat3$activity == "QUIET"] <- "quiet"
bhat3$word.length[bhat3$word.length == "S"] <- "short"
bhat3$word.length[bhat3$word.length == "L"] <- "long"
bhat3$trial <- (bhat3$block-1)*max(bhat3$trial) + bhat3$trial

bhatarah09c <- bhat3
bhatarah09c$exp <- 3
bhatarah09c <- bhatarah09c %>% select(exp, subj, block, trial, task, activity,
                                      word.length, serpos, output, recalled, spos.correct)

save(bhatarah09c, file = "./pkg/data/bhatarah09c.rda")

### Immediate serial recall
pd <- aggregate(spos.correct ~ serpos+activity+word.length,
                data = bhatarah09c[which(bhatarah09c$task == "ISR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Immediate Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short" & pd$activity == "articulatory suppression"],
      y = pd$spos.correct[pd$word.length == "short" &
                            pd$activity == "articulatory suppression"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "long" & pd$activity == "articulatory suppression"],
      y = pd$spos.correct[pd$word.length == "long"
                          & pd$activity == "articulatory suppression"],
      type = "b", lty = 1, pch = 17, col = "green")
lines(x = pd$serpos[pd$word.length == "short" & pd$activity == "quiet"],
      y = pd$spos.correct[pd$word.length == "short" & pd$activity == "quiet"],
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "long" & pd$activity == "quiet"],
      y = pd$spos.correct[pd$word.length == "long" & pd$activity == "quiet"],
      type = "b", lty = 2, pch = 17, col = "green")
legend(8, 1.0, c("AS-short","AS-long",
                 "quiet-short","quiet-long"), lty = c(1,1,2,2),
       pch=c(15,17,15,17), title = "Cue condition:",
       col = c("blue","green","blue","green"), horiz = F, cex = 0.6,
       yjust = 1, xjust = 1)

#### Experiment 4

d4 <- read_excel(paste0(pth, "BhatarahWardSmith&Hayes(2009).xls"),
                 sheet = "Experiment 4",
                 range = "A1:H4321")
d4$trial[d4$task == "ISR"] <- max(d4$trial)+d4$trial[d4$task == "ISR"]

bhat4 <- d4 %>% rename(word.length = wordlength, words = word)
bhat4$recalled <- 1
bhat4$recalled[is.na(bhat4$output)] <- 0
bhat4$spos.correct <- 0
bhat4$spos.correct[bhat4$serpos == bhat4$output] <- 1
bhat4$word.length[bhat4$word.length == "S"] <- "short"
bhat4$word.length[bhat4$word.length == "M"] <- "medium"
bhat4$word.length[bhat4$word.length == "L"] <- "long"
bhat4$exp <- 4

bhatarah09d <- bhat4 %>% select(exp, subj, trial, task, listlength, word.length,
                                serpos, words, output, recalled, spos.correct)
save(bhatarah09d, file = "./pkg/data/bhatarah09d.rda")

### Immediate serial recall
pd <- aggregate(spos.correct ~ serpos+word.length,
                data = bhatarah09d[which(bhatarah09d$task == "ISR"),], FUN = mean)
plot(c(1,6), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Immediate Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short"],
      y = pd$spos.correct[pd$word.length == "short"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium"],
      y = pd$spos.correct[pd$word.length == "medium"],
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long"],
      y = pd$spos.correct[pd$word.length == "long"],
      type = "b", lty = 3, pch = 18, col = "green")
legend(6, 1.0, c("short","medium","long"), lty = 1:3,
       pch=c(15,17,18), title = "Cue condition:", col = c("blue", "red","green"),
       horiz = F, cex = 0.6, yjust = 1, xjust = 1)


### Experiment 5:

d5 <- read_excel(paste0(pth, "BhatarahWardSmith&Hayes(2009).xls"),
                 sheet = "Experiment 5",
                 range = "A1:H8641")
d5 <- d5 %>% rename(word.length = wordlength, words = word, output = `output order`)
d5$trial[d5$task == "ISR"] <- max(d5$trial)+d5$trial[d5$task == "ISR"]

bhat5 <- d5
bhat5$recalled <- 1
bhat5$recalled[is.na(bhat5$output)] <- 0
bhat5$spos.correct <- 0
bhat5$spos.correct[bhat5$serpos == bhat5$output] <- 1
bhat5$word.length[bhat5$word.length == "S"] <- "short"
bhat5$word.length[bhat5$word.length == "M"] <- "medium"
bhat5$word.length[bhat5$word.length == "L"] <- "long"
bhat5$exp <- 5

bhatarah09e <- bhat5 %>% select(exp, subj, trial, task, listlength, word.length,
                                serpos, words, output, recalled, spos.correct)

save(bhatarah09e, file = "./pkg/data/bhatarah09e.rda")

## Reproduction of Figure 8A in Bhatarah et al. (2009)
### Immediate serial recall
pd <- aggregate(recalled ~ serpos+word.length,
                data = bhatarah09e[which(bhatarah09e$task == "FR"),], FUN = mean)
plot(c(1,12), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Free Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = levels(pd$serpos),
    cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short"],
      y = pd$recalled[pd$word.length == "short"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium"],
      y = pd$recalled[pd$word.length == "medium"],
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long"],
      y = pd$recalled[pd$word.length == "long"],
      type = "b", lty = 3, pch = 18, col = "green")
legend(1, 1.0, c("short","medium","long"), lty = 1:3,
       pch=c(15,17,18), title = "Cue condition:", col = c("blue", "red","green"),
       horiz = F, cex = 0.6, yjust = 1, xjust = 0)

