## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM5 Multiple Demands #####################
pth  <- "BenchmarksWM.Data/BM5.1.MultipleMemSets/"

### BM5.1 MultipleMemSets

d=read.csv(paste0(pth, "Cowan.Morey.2007.csv"), header = T, stringsAsFactors = F, strip.white = T)
aggd <- aggregate(response.ACC ~ probeDomain + Domain + Subject, data=d, FUN="mean")

d <- d %>% rename(subj = Subject, age = Age, sex = Sex, trial = Trial,
             condition = Domain, ISI = ISIDur, listlength = ListLength,
             setsize = SetSize, serpos = VerbProbePos, order = presented,
             probe = probeDomain, testpos = probeOrder, acc = response.ACC,
             corr_resp = response.CRESP, resp = response.RESP, rt = response.RT)
d <- d %>% select(subj, age, sex, trial, condition, ISI, listlength, setsize,
                  serpos, order, cue, probe, testpos, acc, corr_resp, resp, rt)

cowan07 <- d %>% rename(testset = testpos)
save(cowan07, file = "./pkg/data/cowan07.rda")

### Reproduce Figure 12 in Oberauer et al. (2018)
pd = aggregate(acc ~ probe * condition, data = cowan07, FUN = mean)
pd$pcode[pd$probe == "Verb"] <- 1
pd$pcode[pd$probe == "Vis"] <- 2

plot(c(0.8,2.2), c(0.5,1.0), type = "n", xlab = "Tested Domain",
     ylab = "Proportion correct", main = "Multiple Memory Sets", xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Verb","Vis"))
lines(x = pd$pcode[pd$condition == "single"], y = pd$acc[pd$condition == "single"],
      type = "b", lty = 1, pch = 15)
lines(x = pd$pcode[pd$condition == "cross"], y = pd$acc[pd$condition == "cross"],
      type = "b", lty = 2, pch = 16)
lines(x = pd$pcode[pd$condition == "within"], y = pd$acc[pd$condition == "within"],
      type = "b", lty = 3, pch = 17)
legend(1, 0.5, c("single", "cross", "within"), lty = 1:3, pch=15:17,
       horiz = F, cex = 0.7, yjust = 0, xjust = 0)

####### BM 5.2 Multiple Task Effects

pth  <- "BenchmarksWM.Data/BM5.2.MultipleTasks/"
#obtain data
chein <- read.table(paste0(pth, "Chein.XSPANdata.txt"),header=TRUE,sep=",")
chein$TEST <- ordered(chein$TEST,levels=c("LEX.LET","SYM.LET","LET", "SYM.LOC","LEX.LOC", "LOC"))

aggregate(SCORES ~ TEST+MemoryType, data = chein, FUN = mean)
chein11 <- chein %>% rename(task = TEST, stimuli = MemoryType, acc = SCORES)

levels(chein11$task)[levels(chein11$task) == "LEX.LET"] <- "lexical"
levels(chein11$task)[levels(chein11$task) == "SYM.LET"] <- "symmetry"
levels(chein11$task)[levels(chein11$task) == "LET"] <- "none"
levels(chein11$task)[levels(chein11$task) == "SYM.LOC"] <- "symmetry"
levels(chein11$task)[levels(chein11$task) == "LEX.LOC"] <- "lexical"
levels(chein11$task)[levels(chein11$task) == "LOC"] <- "none"

levels(chein11$stimuli)[levels(chein11$stimuli) == "LETTER"] <- "letters"
levels(chein11$stimuli)[levels(chein11$stimuli) == "LOCATION"] <- "locations"

chein11 <- chein11 %>% select(stimuli, task, acc)
save(chein11, file = "./pkg/data/chein11.rda")

pd <- aggregate(acc ~ task*stimuli, data = chein11, FUN = mean)
pd <- pd[c(1,2,3,5,4,6),] ## should be lex -> sym for verbal and sym -> let for spatial
barspacing <- c(0.2,0.2,0.2,0.7,0.2,0.2)
legendtext <- c("Lexical", "Symmetry", "None", "Symmetry", "Lexical", "None")
bp = barplot(pd$acc, space = barspacing, col=c(rep("dark gray",3),rep("light gray",3)),
        ylab="Mean Number of Items Recalled", xlab="Distractor Task", axisnames=F,
        ylim = c(1.9,4.5), xpd = F)
text (2,4.2,"Verbal Memory",cex=1)
text (6,4.2,"Spatial Memory",cex=1)
box()
axis(1, at = bp[,1], labels=legendtext, cex.axis=0.7)

### Jarrold (2010) prepare


CondNames <- c("ZeroSix", "OneFive", "TwoFour", "ThreeThree", "FourTwo", "FiveOne", "ZeroSix")
CondCols <- c(3, 8, 13, 19, 24, 29, 34)

Data <- as.data.frame(matrix(NA, 53*7*10*6, 6))
names(Data) <- c("id", "trial", "condition", "serpos", "item", "response")

excluded <- c(1, 19, 27, 29, 33, 35, 37, 43)  # these participants were excluded by Jarrold et al.
Data <- Data[Data$id != excluded, ]

rowNum <- 1
for (subj in 1:53) {
  d = as.data.frame(read_excel(paste0(pth, "Jarrold2010.detailed.xls"), sheet=subj, range="A1:AL69"))
  for (cond in 1:7) {
    for (trial in 1:10) {
      trialname = paste0("T", trial)
      subdat <- subset(d, Trial==trialname)
      for (serpos in 1:6) {
        condition <- names(d)[CondCols[cond]]
        Data[rowNum, 1] <- subj
        Data[rowNum, 2] <- trial
        Data[rowNum, 3] <- condition
        Data[rowNum, 4] <- serpos
        Data[rowNum, 5] <- subdat[serpos, CondCols[cond]]
        resp <- subdat[serpos, CondCols[cond]+1]
        response <- resp  # default
        if (resp == "c") response <- Data[rowNum, 5]  # correct responses are coded as "c", so set response to the list item
        if (resp == "x") response <- "omission"
        if (is.na(resp)) response <- "omission"
        Data[rowNum, 6] <- response
        rowNum <- rowNum + 1
      }
    }
  }
}

write.table(Data, file="Jarrold2010.long.txt", row.names=F)

d = as.data.frame(read_excel(paste0(pth, "Jarrold2010.detailed.xls"), sheet=50, range="A1:AL69"))




#########

jar <- read.table(paste0(pth, "Jarrold2010.long.txt"), header = T)
excluded <- c(1, 19, 27, 29, 33, 35, 37, 43)  # these participants were excluded by Jarrold et al.
jar$id[jar$id == 45] <- 47
jar$id[jar$id == 46] <- 49
jar$id[jar$id == 47] <- 50
jar$id[jar$id == 48] <- 51
jar$id[jar$id == 49] <- 52
jar$id[jar$id == 50] <- 53

jar$group[jar$id %in% rhyme_ids] <- "rhyme"
jar$group[jar$id %in% symm_ids] <- "symmetry"

jar <- jar[which(!is.na(jar$group)),]

jar$response <- as.character(jar$response)
jar$response[jar$response == "Omission"] <- "omission"

jar$correct <- 1
jar$correct[jar$response == "omission"] <- 0
jar$correct[jar$response != jar$item] <- 0

jar$condition <- ordered(jar$condition, levels = c("SixZero", "FiveOne", "FourTwo",
                                              "Three","TwoFour","OneFive","ZeroSix"))

jar_rhyme <- jar[which(jar$group == "symmetry"),]

## Overall Figure
pd <- aggregate(correct ~ cond+condition+group, data = jar, FUN = mean)


plot(c(1,7), c(0.0,1.0), type = "n", xlab = "Condition",
     ylab = "Proportion correct", main = "Positions of Processing Interval
     in Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7), labels = levels(pd$condition), cex.axis = 0.7)
lines(x = pd$cond[pd$group == "rhyme"], y = pd$correct[pd$group == "rhyme"],
      type = "b", lty = 1, pch = 15, col = "red")
lines(x = pd$cond[pd$group == "symmetry"], y = pd$correct[pd$group == "symmetry"],
      type = "b", lty = 2, pch = 17, col = "blue")
legend(7, 0, c("Rhyme", "Symmetry"), lty = 1:2, pch=c(15,17), title = "Processing Task:",
       col = c("red", "blue"), horiz = F, cex = 0.6, yjust = 0, xjust = 1)


### get position errors

id_vec <- sort(unique(jar$id))
trial_vec <- sort(unique(jar$trial))
cond_vec <- sort(unique(jar$cond))

jar$item <- as.character(jar$item)

jar$recalled <- 0
for (subj in id_vec) {
  for (tr in trial_vec) {
    for (c in cond_vec) {
      words <- jar$item[jar$id == subj & jar$trial == tr & jar$cond == c]
      for (sp in 1:6) {
        resp <- jar$response[jar$id == subj & jar$trial == tr &
                               jar$cond == c & jar$serpos == sp]
        corr <- jar$correct[jar$id == subj & jar$trial == tr &
                               jar$cond == c & jar$serpos == sp]
        if (corr == 1) {
          jar$recalled[jar$id == subj & jar$trial == tr &
                        jar$cond == c & jar$serpos == sp] <- 1
        } else if (corr == 0) {
          if (resp == "omission") {
            jar$recalled[jar$id == subj & jar$trial == tr &
                           jar$cond == c & jar$serpos == sp] <- 0
          } else if (resp %in% words) {
            jar$recalled[jar$id == subj & jar$trial == tr &
                           jar$cond == c & jar$serpos == sp] <- 1
          } else {
            jar$recalled[jar$id == subj & jar$trial == tr &
                           jar$cond == c & jar$serpos == sp] <- 0
          }
        }
      }
    }
  }
}

jar$transposition <- 0
jar$transposition[jar$recalled == 1 & jar$correct == 0] <- 1

jarrold10 <- jar %>% select(id, trial, group, cond, condition, serpos,
                            item, response, correct, recalled, transposition)

levels(jarrold10$condition)[levels(jarrold10$condition) == "Three"] <- "ThreeThree"

## Andy's fix code:
jarrold10$response <- stri_trans_general(jarrold10$response, "latin-ascii")
jarrold10$response <- iconv(jarrold10$response, from="UTF-8", to="ASCII")
unique(jarrold10$response)

save(jarrold10, file = "./pkg/data/jarrold10.rda")

#### Klauer & Zhao (2004)

rm(list = ls())
pth  <- "BenchmarksWM.Data/BM5.2.MultipleTasks/"

Data1 <- NULL
Data2 <- NULL
Data4 <- NULL
Nsubj <- c(20, 24, 18)

### Read data of Experiments 1 and 2

for (experiment in 1:2) {

  maxT2trials <- 16

  for (id in 1:Nsubj[experiment]) {

    filename = paste0(pth, "/Klauer.Zhao.2004/exp", experiment, "/RAUS.", id)
    ncol <- count.fields(filename, skip=1)
    data <- read.table(filename, header=F, skip=1, fill=T, col.names=1:max(ncol))

    D <- as.data.frame(matrix(NA, dim(data)[1]/2, 9 + 3*maxT2trials))
    varnames <- c("id", "trial", "T1", "T2", "T1prestime", "T1stim", "T1resp", "T1correct", "T2num")
    for (t2 in 1:maxT2trials) {
      varnames <- c( varnames, c(paste0("T2stim", t2), paste0("T2resp", t2), paste0("T2correct", t2)) )
    }
    names(D) <- varnames

    trial <- 1
    for (line in 1:dim(data)[1]) {
      task <- 2 - (line %% 2) # odd lines -> task 1, even lines -> task 2
      if (task == 1) {
        D[trial, "id"] <- id
        D[trial, "trial"] <- trial
        D[trial, c("T1", "T1prestime", "T1stim", "T1resp")] <- data[line, 1:4]
        D[trial, "T1correct"] = D[trial,"T1stim"] == D[trial,"T1resp"]
      }
      if (task == 2) {
        D[trial, "T2"] <- data[line, 1]
        if (data[line, 1] > 0) {
          D[trial, "T2num"] <- data[line, 2]
          a <- 3
          b <- 4
          for (t2 in 1:data[line, 2]) {
            D[trial, c(paste0("T2stim", t2), paste0("T2resp", t2))] <- data[line, a:b]
            if (D[trial, "T2"] == 1) D[trial, paste0("T2correct", t2)] = data[line,a] == data[line,b]
            # T2 = 1: movement task: correct if stim == response
            if (D[trial, "T2"] == 2) D[trial, paste0("T2correct", t2)] = ceiling(data[line,a]/8) == data[line,b]
            # T2 = 2: color task: correct for stim < 8 and response = 1, and for stim > 8 and response = 2
            a <- a+2
            b <- b+2
          }
        }
        trial <- trial + 1
      }
    }

    if (experiment == 1) Data1 <- rbind(Data1, D)
    if (experiment == 2) Data2 <- rbind(Data2, D)
  }
}

### Encode Experiment 4
# T1: task 1, 0 = locations, 1 = Chinese ideographs
# T2: task 2, 0 = none, 1 = movement (tapping), 2 = colors. Raw data from the movement task were lost -> aggregate data are read separately

maxT2trials <- 16

for (id in 1:Nsubj[3]) {

  filename = paste0(pth, "Klauer.Zhao.2004/exp4/RAUS.", id)
  ncol <- count.fields(filename, skip=1)
  data <- read.table(filename, header=F, skip=1, fill=T, col.names=1:max(ncol))

  D <- as.data.frame(matrix(NA, dim(data)[1]/2, 9 + 3*maxT2trials))
  varnames <- c("id", "trial", "T1", "T2", "T1prestime", "T1stim", "T1resp", "T1correct", "T2num")
  for (t2 in 1:maxT2trials) {
    varnames <- c( varnames, c(paste0("T2stim", t2), paste0("T2resp", t2), paste0("T2correct", t2)) )
  }
  names(D) <- varnames

  trial <- 1
  for (line in 1:dim(data)[1]) {
    task <- 2 - (line %% 2) # odd lines -> task 1, even lines -> task 2
    if (task == 1) {
      D[trial, "id"] <- id
      D[trial, "trial"] <- trial
      D[trial, c("T1", "T1prestime", "T1stim", "T1resp")] <- data[line, 1:4]
      D[trial, "T1correct"] = D[trial,"T1stim"] == D[trial,"T1resp"]
    }
    if (task == 2) {
      D[trial, "T2"] <- data[line, 1]
      if (data[line, 1] == 2) {   # only for T2 = 2 (colors task) the T2 data are in this file
        D[trial, "T2num"] <- data[line, 2]
        a <- 3
        b <- 4
        for (t2 in 1:data[line, 2]) {
          D[trial, c(paste0("T2stim", t2), paste0("T2resp", t2))] <- data[line, a:b]
          D[trial, paste0("T2correct", t2)] = ceiling(data[line,a]/8) == data[line,b]
          # T2 = 2: color task: correct for stim < 8 and response = 1, and for stim > 8 and response = 2
          a <- a+2
          b <- b+2
        }
      }
      trial <- trial + 1
    }
  }

  Data4 <- rbind(Data4, D)
}

# Read aggregate data of accuracy in the movement (tapping) task
Data4T2 <- read.table(paste0(pth,"Klauer.Zhao.2004/exp4/data1-18.dat"), header=T)
names(Data4T2) <- c("id",
                    "T1corr.t1fast,t1loc.t2none", "T1corr.t1fast,t1loc.t2move", "T1corr.t1fast,t1loc.t2col",
                    "T1corr.t1fast,t1ideo.t2none", "T1corr.t1fast,t1ideo.t2move", "T1corr.t1fast,t1ideo.t2col",
                    "T1corr.t1medium,t1loc.t2none", "T1corr.t1medium,t1loc.t2move", "T1corr.t1medium,t1loc.t2col",
                    "T1corr.t1medium,t1ideo.t2none", "T1corr.t1medium,t1ideo.t2move", "T1corr.t1medium,t1ideo.t2col",
                    "T1corr.t1slow,t1loc.t2none", "T1corr.t1slow,t1loc.t2move", "T1corr.t1slow,t1loc.t2col",
                    "T1corr.t1slow,t1ideo.t2none", "T1corr.t1slow,t1ideo.t2move", "T1corr.t1slow,t1ideo.t2col",
                    "T2corr.t1fast.t1loc.t2move", "T2corr.t1fast.t1loc.t2col",
                    "T2corr.t1fast.t1ideo.t2move", "T2corr.t1fast.t1ideo.t2col",
                    "T2corr.t1medium.t1loc.t2move", "T2corr.t1medium.t1loc.t2col",
                    "T2corr.t1medium.t1ideo.t2move", "T2corr.t1medium.t1ideo.t2col",
                    "T2corr.t1slow.t1loc.t2move", "T2corr.t1slow.t1loc.t2col",
                    "T2corr.t1slow.t1ideo.t2move", "T2corr.t1slow.t1ideo.t2col")
# Mean accuracies of T1, for 3 levels of presentation duration of T1 (fast, medium, slow), 2 levels of kind of T1 (location, Chinese ideographs), and 3 levels of kind of T2 (none, movement, color), followed by
# Mean accuracies of T2, for 3 levels of presentation duration of T1 (fast, medium, slow), 2 levels of kind of T1 (location, Chinese ideographs), and 2 levels of T2 (movement, color).
# For the movement task, the accuracy is the average number of long tap pauses multiplied by 100


## Experiment 1
dd <- Data1
dd$memory_task[dd$T1 == 0] <- "locations"
dd$memory_task[dd$T1 == 1] <- "ideographs"

dd$processing[dd$T2 == 0] <- "none"
dd$processing[dd$T2 == 1] <- "spatial tapping"
dd$processing[dd$T2 == 2] <- "visual"

dd$domain <- "none"
dd$domain[dd$T1 == 0 & dd$T2 == 1] <- "same"
dd$domain[dd$T1 == 1 & dd$T2 == 2] <- "same"
dd$domain[dd$T1 == 0 & dd$T2 == 2] <- "different"
dd$domain[dd$T1 == 1 & dd$T2 == 1] <- "different"

names(dd)[names(dd) == "T1prestime"] <- "prestime"

dd$correct <- 0
dd$correct[dd$T1correct == "TRUE"] <- 1

### Compute an accuracy score for the processing tasks

id_vec <- sort(unique(dd$id))
trial_vec <- sort(unique(dd$trial))
dd$acc <- 0
for (subj in id_vec) {
  for (tr in trial_vec) {
    num <- dd$T2num[dd$id == subj & dd$trial == tr]
    if (is.na(num)) {
      dd$acc[dd$id == subj & dd$trial == tr] <- NA
    } else {
      counter <-  0
      for (div in 1:num) {
        name <- paste0("T2correct", div)
        subdata <- subset(dd, id == subj & trial == tr)
        if (subdata[names(subdata) == name] == "TRUE") {
          counter <-  counter + 1
        } else {
          counter <- counter
        }
      }
      dd$acc[dd$id == subj & dd$trial == tr] <- counter/num
    }
  }
}

names(dd)[names(dd) == "T2num"] <- "num"

dd$exp <- "Exp1"

exp1 <- dd %>% select(exp, id, trial, memory_task, processing, domain, prestime,
                      num, correct, acc)

## Experiment 2

dd <- Data2
dd$memory_task[dd$T1 == 0] <- "locations"
dd$memory_task[dd$T1 == 1] <- "ideographs"

dd$processing[dd$T2 == 0] <- "none"
dd$processing[dd$T2 == 1] <- "spatial tapping"
dd$processing[dd$T2 == 2] <- "visual"

dd$domain <- "none"
dd$domain[dd$T1 == 0 & dd$T2 == 1] <- "same"
dd$domain[dd$T1 == 1 & dd$T2 == 2] <- "same"
dd$domain[dd$T1 == 0 & dd$T2 == 2] <- "different"
dd$domain[dd$T1 == 1 & dd$T2 == 1] <- "different"

names(dd)[names(dd) == "T1prestime"] <- "prestime"

dd$correct <- 0
dd$correct[dd$T1correct == "TRUE"] <- 1

### Compute an accuracy score for the processing tasks

id_vec <- sort(unique(dd$id))
trial_vec <- sort(unique(dd$trial))
dd$acc <- 0
for (subj in id_vec) {
  for (tr in trial_vec) {
    num <- dd$T2num[dd$id == subj & dd$trial == tr]
    if (is.na(num)) {
      dd$acc[dd$id == subj & dd$trial == tr] <- NA
    } else {
      counter <-  0
      for (div in 1:num) {
        name <- paste0("T2correct", div)
        subdata <- subset(dd, id == subj & trial == tr)
        if (subdata[names(subdata) == name] == "TRUE") {
          counter <-  counter + 1
        } else {
          counter <- counter
        }
      }
      dd$acc[dd$id == subj & dd$trial == tr] <- counter/num
    }
  }
}

names(dd)[names(dd) == "T2num"] <- "num"

dd$exp <- "Exp2"

exp2 <- dd %>% select(exp, id, trial, memory_task, processing, domain, prestime,
                      num, correct, acc)

## Experiment 4
dd <- Data4
dd$memory_task[dd$T1 == 0] <- "locations"
dd$memory_task[dd$T1 == 1] <- "ideographs"

dd$processing[dd$T2 == 0] <- "none"
dd$processing[dd$T2 == 1] <- "spatial tapping"
dd$processing[dd$T2 == 2] <- "visual"

dd$domain <- "none"
dd$domain[dd$T1 == 0 & dd$T2 == 1] <- "same"
dd$domain[dd$T1 == 1 & dd$T2 == 2] <- "same"
dd$domain[dd$T1 == 0 & dd$T2 == 2] <- "different"
dd$domain[dd$T1 == 1 & dd$T2 == 1] <- "different"

names(dd)[names(dd) == "T1prestime"] <- "prestime"

dd$correct <- 0
dd$correct[dd$T1correct == "TRUE"] <- 1

### Compute an accuracy score for the processing tasks

id_vec <- sort(unique(dd$id))
trial_vec <- sort(unique(dd$trial))
dd$acc <- 0
for (subj in id_vec) {
  for (tr in trial_vec) {
    num <- dd$T2num[dd$id == subj & dd$trial == tr]
    if (is.na(num)) {
      dd$acc[dd$id == subj & dd$trial == tr] <- NA
    } else {
      counter <-  0
      for (div in 1:num) {
        name <- paste0("T2correct", div)
        subdata <- subset(dd, id == subj & trial == tr)
        if (subdata[names(subdata) == name] == "TRUE") {
          counter <-  counter + 1
        } else {
          counter <- counter
        }
      }
      dd$acc[dd$id == subj & dd$trial == tr] <- counter/num
    }
  }
}

names(dd)[names(dd) == "T2num"] <- "num"

for (subj in id_vec) {
  fl <- dd$id == subj & dd$prestime == 225 &
    dd$memory_task == "locations" & dd$processing == "spatial tapping"
  dd$acc[fl] <- Data4T2$T2corr.t1fast.t1loc.t2move[Data4T2$id == subj]
  fi <- dd$id == subj & dd$prestime == 225 &
    dd$memory_task == "ideographs" & dd$processing == "spatial tapping"
  dd$acc[fi] <- Data4T2$T2corr.t1fast.t1ideo.t2move[Data4T2$id == subj]
  ml <- dd$id == subj & dd$prestime == 281 &
    dd$memory_task == "locations" & dd$processing == "spatial tapping"
  dd$acc[ml] <- Data4T2$T2corr.t1medium.t1loc.t2move[Data4T2$id == subj]
  mi <- dd$id == subj & dd$prestime == 281 &
    dd$memory_task == "ideographs" & dd$processing == "spatial tapping"
  dd$acc[mi] <- Data4T2$T2corr.t1medium.t1ideo.t2move[Data4T2$id == subj]
  sl <- dd$id == subj & dd$prestime == 337 &
    dd$memory_task == "locations" & dd$processing == "spatial tapping"
  dd$acc[sl] <- Data4T2$T2corr.t1slow.t1loc.t2move[Data4T2$id == subj]
  si <- dd$id == subj & dd$prestime == 337 &
    dd$memory_task == "ideographs" & dd$processing == "spatial tapping"
  dd$acc[si] <- Data4T2$T2corr.t1slow.t1ideo.t2move[Data4T2$id == subj]
}

dd$acc[dd$acc == 0 & dd$processing == "spatial tapping"] <- 1.0
dd$acc[dd$acc == 1667 & dd$processing == "spatial tapping"] <- 1-(0.1667)

dd$exp <- "Exp4"

exp4 <- dd %>% select(exp, id, trial, memory_task, processing, domain, prestime,
                      num, correct, acc)

klauer04 <- rbind(exp1,exp2,exp4)

klauer04 <- klauer04 %>% rename(subj = id)

klauer04$num[klauer04$exp == "Exp4" & klauer04$processing == "spatial tapping"] <- 11.2

save(klauer04, file="./pkg/data/klauer04.rda")

## Reproduce Figure 2 in Klauer & Zhao (2004)

d <- subset(klauer04, exp == "Exp1")
pd <- aggregate(correct ~ processing*memory_task, data = d, FUN = mean)

barspacing <- c(0.2,0.2,0.2,0.7,0.2,0.2)
legendtext <- c("None", "Spatial", "Visual", "None", "Spatial", "Visual")
bp = barplot(pd$correct, space = barspacing, col=c(rep("dark gray",3),rep("light gray",3)),
             ylab="Proportion correct", xlab="Distractor Task", axisnames=F,
             ylim = c(0.6,1.0), xpd = F, main = "Effects of Task Domains")
text (2,.95,"Visual Memory",cex=1)
text (6,.95,"Spatial Memory",cex=1)
box()
axis(1, at = bp[,1], labels=legendtext, cex.axis=0.7)


###################
### Barrouillet et al. 2007
rm(list = ls())

pth  <- "BenchmarksWM.Data/BM5.2.4.CogLoad/"

#remap levels into consistent nomenclature after gathering
remap <- function (span, vnames) {
  s<-span$'Cognitive Load'
  s[span$'Cognitive Load'==vnames[1]] <- "low"
  s[span$'Cognitive Load'==vnames[2]] <- "medium"
  s[span$'Cognitive Load'==vnames[3]] <- "high"
  return(s)
}

#total time available for distractor
totaltime <- 6900

#extract the rows that contain data
temp <- read_excel(paste0(pth, "Barrouillet.E3.xls"),sheet=1,col_names=F) [c(2:17,32:47),]
#convert into numbers
b07 <- as.data.frame(lapply(temp,FUN=function(x) as.numeric(as.character(x))))
#add identifier for task
b07$task <- c(rep("parity",16),rep("location",16))

names(b07) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                "X10", "X11", "X12", "X13", "task")

#now convert into suitable format
span<-gather(b07,"Cognitive Load","Mean Span",c(5,9,13)) [,c("X1","task","Cognitive Load","Mean Span")]
span$'Cognitive Load' <- remap(span,c("X5","X9","X13"))

tontask<-gather(b07,"Cognitive Load","Time on task",c(2,6,10)) [,c("X1","task","Cognitive Load","Time on task")]
tontask$'Cognitive Load' <- remap(tontask,c("X2","X6","X10"))

pcnr <-gather(b07,"Cognitive Load","Pct NR",X3,X7,X11) [,c("X1","task","Cognitive Load","Pct NR")]
pcnr$'Cognitive Load' <- remap(pcnr,c("X3","X7","X11"))

serfait <- gather(b07,"Cognitive Load","Ser Fait",X4,X8,X12) [,c("X1","task","Cognitive Load","Ser Fait")]
serfait$'Cognitive Load' <- remap(serfait,c("X4","X8","X12"))

#merge all dependent measures together with identifiers into final data set
b07fin<-Reduce(function(...) merge(...),list(span,tontask,pcnr,serfait))
names(b07fin)[1] <- "participant"

funpth <- "BenchmarksWM.Data/Functions/"
source(paste(funpth, "/BakemanL.R", sep=""))
library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)

b07fin <- BakemanL(b07fin, id="participant", dv="Mean Span")

#average across participants for display
b07means <- aggregate(cbind(`Time on task`,`Mean Span`)~ task + `Cognitive Load`, data=b07fin, FUN=mean)
b07SE <- aggregate(cbind(`Time on task`,`Mean Span`)~ task + `Cognitive Load`, data=b07fin, FUN=function(x) {sd(x)/sqrt(length(x))})
b07means$'Time on task' <- b07means$'Time on task'/totaltime

#now do some plotting
p2f <- 0
if (p2f) {
  pdf(file="barrouillet07.pdf",height=6,width=6)
} else {x11(height=6,width=6)}

plot(0,0, xlim=c(0.2,.6),ylim=c(0,7), type="n", las=1,
     xlab="Total Processing Time/Total Time", ylab="Mean Span",cex.lab=1)
abline(lm(`Mean Span` ~ `Time on task`, data=filter(b07means, task=="parity"))$coefficients,lwd=2.5)
abline(lm(`Mean Span` ~ `Time on task`, data=filter(b07means, task=="location"))$coefficients,lwd=2.5,lty="dashed",col="gray")
xx <- filter(b07means, task=="location")$'Time on task'
mn <- filter(b07means, task=="location")$'Mean Span'
se <- filter(b07SE, task=="location")$'Mean Span'
points(xx,mn,cex=1.3,pch=21,bg="gray")
arrows(xx,mn-1.96*se, xx, mn+1.96*se, length=0.05, angle=90, code=3)
xx <- filter(b07means, task=="parity")$'Time on task'
mn <- filter(b07means, task=="parity")$'Mean Span'
se <- filter(b07SE, task=="parity")$'Mean Span'
points(xx,mn,cex=1.3,pch=22,bg="black")
arrows(xx,mn-1.96*se, xx, mn+1.96*se, length=0.05, angle=90, code=3)
legend(.3,2,c("Parity","Location"),lty=c("solid","dashed"),pch=c(22,21),pt.bg=c("gray","black"), pt.cex=1.2)

if (p2f) {dev.off()}

barrouillet07 <- b07fin

barrouillet07 <- barrouillet07 %>% dplyr::rename(subj = participant, cogload = `Cognitive Load`,
                                          span = `Mean Span`, tasktime = `Time on task`,
                                          pctNR = `Pct NR`, serfait = `Ser Fait`)

barrouillet07$CL <- barrouillet07$tasktime/totaltime
barrouillet07$cogload <- as.factor(barrouillet07$cogload)
barrouillet07$cogload <- ordered(barrouillet07$cogload, levels = c("high",
                                                                   "medium",
                                                                   "low"))
barrouillet07 <- barrouillet07 %>% dplyr::rename(series_completed = serfait)

barrouillet07 <- barrouillet07 %>% dplyr::select(subj, task, cogload, CL, tasktime,
                                                 series_completed, span, pctNR)
save(barrouillet07, file = "./pkg/data/barrouillet07.rda")

pd <- aggregate(cbind(span, CL) ~ task + cogload, data = barrouillet07, FUN = mean)

plot(c(0.25,0.6), c(3.0,6.5), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6), labels = T)
lines(x = pd$CL[pd$task == "parity"], y = pd$span[pd$task == "parity"],
      type = "b", lty = 1, pch = 15)
lines(x = pd$CL[pd$task == "location"], y = pd$span[pd$task == "location"],
      type = "b", lty = 2, pch = 17)
abline(lm(span ~ CL, data = pd[which(pd$task == "parity"),])$coefficients,
       lty = 1, pch = 15)
abline(lm(span ~ CL, data = pd[which(pd$task == "location"),])$coefficients,
       lty = 2, pch = 16)
legend(0.6, 6.5, c("Parity", "Location"), lty = 1:4, pch=15:18, title = "Task:",
       horiz = F, cex = 0.7, yjust = 1, xjust = 1)




##### More CL: Barrouillet et al. (2011)

# Parity judgment experiment
Parity4 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="B17:D33")
names(Parity4) <- c("subject", "ptime", "span")
Parity4$task <- "Parity-4"
Parity6 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="E17:F33")
Parity6 <- cbind(Parity4[,"subject"], Parity6)
names(Parity6) <- c("subject", "ptime", "span")
Parity6$task <- "Parity-6"
Parity8 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="G17:H33")
Parity8 <- cbind(Parity4[,"subject"], Parity8)
names(Parity8) <- c("subject", "ptime", "span")
Parity8$task <- "Parity-8"
Parity <- rbind(Parity4, Parity6, Parity8)
Parity$totaltime <- 6900
Parity$CL <- Parity$ptime/Parity$totaltime  # cognitive load = processing time / total time

# Size judgment experiment
Size4 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="B17:D33")
names(Size4) <- c("subject", "ptime", "span")
Size4$task <- "Size-4"
Size6 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="E17:F33")
Size6 <- cbind(Size4[,"subject"], Size6)
names(Size6) <- c("subject", "ptime", "span")
Size6$task <- "Size-6"
Size8 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="G17:H33")
Size8 <- cbind(Size4[,"subject"], Size8)
names(Size8) <- c("subject", "ptime", "span")
Size8$task <- "Size-8"
Size <- rbind(Size4, Size6, Size8)
Size$totaltime <- 6900
Size$CL <- Size$ptime/Size$totaltime

# Color Stroop and number Stroop experiments - for these experiments, processing time is constant
# across participants because it was assessed in a separate pre-test

ColorStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="B13:F33"))
names(ColorStroop) <- c("subject", "ptimeNeutral", "ptimeColor", "spanNeutral", "spanColor")
CStroopNeutral <- ColorStroop[,c("subject", "ptimeNeutral", "spanNeutral")]
names(CStroopNeutral) <- c("subject", "ptime", "span")
CStroopNeutral$task <- "ColorStroopNeutral"
CStroopColor <- ColorStroop[,c("subject", "ptimeColor", "spanColor")]
names(CStroopColor) <- c("subject", "ptime", "span")
CStroopColor$task <- "ColorStroopColor"
ColorStroop <- rbind(CStroopNeutral, CStroopColor)
ColorStroop$totaltime <- 8500
ColorStroop$CL <- ColorStroop$ptime/ColorStroop$totaltime

NumberStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="I13:M33"))
names(NumberStroop) <- c("subject", "ptimeNeutral", "ptimeNumber", "spanNeutral", "spanNumber")
NStroopNeutral <- NumberStroop[,c("subject", "ptimeNeutral", "spanNeutral")]
names(NStroopNeutral) <- c("subject", "ptime", "span")
NStroopNeutral$task <- "NumberStroopNeutral"
NStroopNumber <- NumberStroop[,c("subject", "ptimeNumber", "spanNumber")]
names(NStroopNumber) <- c("subject", "ptime", "span")
NStroopNumber$task <- "NumberStroopNumber"
NumberStroop <- rbind(NStroopNeutral, NStroopNumber)
NumberStroop$totaltime <- 8500
NumberStroop$CL <- NumberStroop$ptime/NumberStroop$totaltime

# Updating experiments
# Span procedure, according to Barrouillet et al:
# "Two units were added to spans to take into account the fact that the processing task
# required the continuous maintenance of two items"

RunningCount <- read_excel("Barrouillet.2011.xlsx",sheet="Updating",range="B14:F33")
names(RunningCount) <- c("subject", "ptimeUpdate", "ptimeSimple", "spanUpdate", "spanSimple")
RCountSimple <- RunningCount[, c("subject", "ptimeSimple", "spanSimple")]
names(RCountSimple) <- c("subject", "ptime", "span")
RCountSimple$task <- "Simple Storage"
RCountUpdate <- RunningCount[, c("subject", "ptimeUpdate", "spanUpdate")]
names(RCountUpdate) <- c("subject", "ptime", "span")
RCountUpdate$task <- "Running Count"
RunningCount <- rbind(RCountSimple, RCountUpdate)
RunningCount$totaltime <- 14000
RunningCount$CL <- RunningCount$ptime/RunningCount$totaltime

Nback <- read_excel("Barrouillet.2011.xlsx",sheet="Updating",range="J14:N38")
names(Nback) <- c("subject", "ptime0back", "ptime2back", "span2back", "span0back")
NB0 <- Nback[, c("subject", "ptime0back", "span0back")]
names(NB0) <- c("subject", "ptime", "span")
NB0$task <- "0-back"
NB2 <- Nback[, c("subject", "ptime2back", "span2back")]
names(NB2) <- c("subject", "ptime", "span")
NB2$task <- "2-back"
Nback <- rbind(NB0, NB2)
Nback$totaltime <- 12500
Nback$CL <- Nback$ptime/Nback$totaltime

Data <- as.data.frame(rbind(Parity, Size, ColorStroop, NumberStroop, RunningCount, Nback))
Data$task <- as.factor(Data$task)

bar <- Data

upd <- c("0-back", "2-back", "Running Count", "Simple Storage")
inh <- c("ColorStroopColor", "ColorStroopNeutral", "NumberStroopNeutral",
         "NumberStroopNumber")
respsel <- c("Size-4", "Size-6", "Size-8")
retr <- c("Parity-4", "Parity-6", "Parity-8")

bar$task_domain <- ""

bar$task_domain[bar$task %in% upd] <- "updating"
bar$task_domain[bar$task %in% inh] <- "inhibition"
bar$task_domain[bar$task %in% respsel] <- "response selection"
bar$task_domain[bar$task %in% retr] <- "retrieval"

barrouillet11 <- bar %>% dplyr::rename(prestime = ptime, subj = subject) %>%
  dplyr::select(subj, task_domain, task, totaltime, prestime, CL, span)

save(barrouillet11, file = "./pkg/data/barrouillet11.rda")

pd <- aggregate(cbind(span, CL) ~ task, data = barrouillet11, FUN = mean)

pt=c(24, 24, 23, 23, 23, 23, 21, 21, 21, 24, 24, 22, 22, 22) # markers corresponding to Barrouillet et al (2011)
col=rep("black", 14)
ptcol=c("red", "red", "blue", "blue", "blue", "blue", "black", "black", "black", "red", "red", "green", "green", "green")
Tasks <- unique(Data$task)
xlim <- c(0,1)
ylim <- c(0,9)

plot(xlim, ylim, type = "n", xlab = "Cognitive Load",
     ylab = "Memory Span", main = "Cognitive Load in different Tasks", xaxt = "n")
axis(side = 1, at = 0:10/10, labels = T)
points(x = pd$CL, y = pd$span,
       type = "p", pch = pt, bg = ptcol)

abline(a=8.13, b=-8.33)
legend(1,9,c("Updating", "Inhibition", "Response Selection", "Retrieval"),
       pch=c(24,23,22,21), pt.bg=c("red", "blue", "green", "black"), yjust=1, xjust=1,
       cex = 0.7)

### Vergauwe 2010

rm(list = ls())
pth <- "BenchmarksWM.Data/BM5.2.4.CogLoad/"

verver <- read_excel(paste0(pth,"Vergauwe et al. 2010.xlsx"),
                     sheet = "verbal-verbal_recall", range = "A3:D27")
spaspa <- read_excel(paste0(pth,"Vergauwe et al. 2010.xlsx"),
                     sheet = "spatial-spatial_recall", range = "A3:D27")
verspa <- read_excel(paste0(pth,"Vergauwe et al. 2010.xlsx"),
                     sheet = "verbal-spatial recall", range = "A3:D27")
spaver <- read_excel(paste0(pth,"Vergauwe et al. 2010.xlsx"),
                     sheet = "spatial-verbal_recall", range = "A3:D27")

verver$memory <- "verbal"
verver$processing <- "verbal"
spaspa$memory <- "spatial"
spaspa$processing <- "spatial"
verspa$memory <- "verbal"
verspa$processing <- "spatial"
spaver$memory <- "spatial"
spaver$processing <- "verbal"

vv <- gather(verver, key = "CL", value = "span", 2:4, factor_key = T)
ss <- gather(spaspa, key = "CL", value = "span", 2:4, factor_key = T)
vs <- gather(verspa, key = "CL", value = "span", 2:4, factor_key = T)
sv <- gather(spaver, key = "CL", value = "span", 2:4, factor_key = T)

vergauwe10 <- rbind(vv,ss,vs,sv)
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span low CL"] <- "low"
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span medium CL"] <- "medium"
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span high CL"] <- "high"


###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2010.xlsx"),
                    sheet = "verbal-verbal_processing", range = "A1:GL865")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1l <- pro_1l %>% select(sub, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1m <- pro_1l %>% select(sub, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1h <- pro_1l %>% select(sub, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "verbal" & processing == "verbal")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- dvv

###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2010.xlsx"),
                    sheet = "spatial-spatial_processing", range = "A1:FF865")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1l <- pro_1l %>% select(sub, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1m <- pro_1l %>% select(sub, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1h <- pro_1l %>% select(sub, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "spatial" & processing == "spatial")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- rbind(verg10,dvv)

###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2010.xlsx"),
                    sheet = "verbal-spatial_processing", range = "A1:GL865")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1l <- pro_1l %>% select(sub, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1m <- pro_1l %>% select(sub, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,3:98], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,99:194], na.rm = T)
p1h <- pro_1l %>% select(sub, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "verbal" & processing == "spatial")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- rbind(verg10,dvv)

###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2010.xlsx"),
                    sheet = "spatial-verbal_processing", range = "A1:FF865")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1l <- pro_1l %>% select(sub, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1m <- pro_1l %>% select(sub, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,3:82], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,83:162], na.rm = T)
p1h <- pro_1l %>% select(sub, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ sub+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "spatial" & processing == "verbal")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- rbind(verg10,dvv)

vergauwe10 <- verg10

vergauwe10 <- vergauwe10 %>% dplyr::rename(meanAcc = acc, meanRT = rt)
vergauwe10 <- vergauwe10 %>% dplyr::rename(subj = sub)
vergauwe10$totaltime <- 0
vergauwe10$totaltime[vergauwe10$CL == "low"] <- 2000
vergauwe10$totaltime[vergauwe10$CL == "medium"] <- 1293
vergauwe10$totaltime[vergauwe10$CL == "high"] <- 1000


vergauwe10$cogload <- vergauwe10$meanRT/vergauwe10$totaltime

aggregate(cbind(span, cogload) ~ CL + memory + processing, data = vergauwe10, FUN = mean)

vergauwe10 <- vergauwe10 %>% select(subj, memory, processing, CL, totaltime, meanRT,
                                    meanAcc, cogload, span)

save(vergauwe10, file = "./pkg/data/vergauwe10.rda")


pd <- aggregate(cbind(span, cogload) ~  CL + memory + processing, data = vergauwe10, FUN = mean)

plot(c(0.2,0.7), c(1.5,6.0), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.2,0.3,0.4,0.5,0.6,0.7), labels = T)
points(x = pd$cogload[pd$memory == "verbal" & pd$processing == "verbal"],
      y = pd$span[pd$memory == "verbal" & pd$processing == "verbal"],
      type = "p", pch = 15, col = "red")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal" &
                                            pd$processing == "verbal"),])$coefficients,
       lty = 1, pch = 15, col = "red")
points(x = pd$cogload[pd$memory == "verbal" & pd$processing == "spatial"],
      y = pd$span[pd$memory == "verbal" & pd$processing == "spatial"],
      type = "p", pch = 16, col = "blue")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal" &
                                            pd$processing == "spatial"),])$coefficients,
       lty = 2, pch = 16, col = "blue")
points(x = pd$cogload[pd$memory == "spatial" & pd$processing == "verbal"],
      y = pd$span[pd$memory == "spatial" & pd$processing == "verbal"],
      type = "p", pch = 17, col = "green")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial" &
                                            pd$processing == "verbal"),])$coefficients,
       lty = 3, pch = 17, col = "green")
points(x = pd$cogload[pd$memory == "spatial" & pd$processing == "spatial"],
      y = pd$span[pd$memory == "spatial" & pd$processing == "spatial"],
      type = "p", pch = 18, col = "black")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial" &
                                            pd$processing == "spatial"),])$coefficients,
       lty = 4, pch = 18, col = "black")
legend(0.2, 1.5, c("verbal-verbal", "verbal-spatial",
                   "spatial-verbal", "spatial-spatial"), lty = 1:4,
       pch=15:18, col = c("red","blue","green","black"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)

############################################################################
### Vergauwe 2012

rm(list = ls())
pth <- "BenchmarksWM.Data/BM5.2.4.CogLoad/"

verspa <- read_excel(paste0(pth,"Vergauwe et al. 2012.xlsx"),
                     sheet = "Exp 1_recall performance", range = "A4:D35")
spaver <- read_excel(paste0(pth,"Vergauwe et al. 2012.xlsx"),
                     sheet = "Exp 2_recall performance", range = "A4:D40")

verspa$memory <- "verbal"
verspa$processing <- "spatial"
spaver$memory <- "spatial"
spaver$processing <- "verbal"

vs <- gather(verspa, key = "CL", value = "span", 2:4, factor_key = T)
sv <- gather(spaver, key = "CL", value = "span", 2:4, factor_key = T)

vergauwe10 <- rbind(vs,sv)
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span low CL"] <- "low"
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span medium CL"] <- "medium"
levels(vergauwe10$CL)[levels(vergauwe10$CL) == "span high CL"] <- "high"


###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2012.xlsx"),
                    sheet = "Exp 1_processing performance", range = "A1:GM1117")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,4:99], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,100:195], na.rm = T)
p1l <- pro_1l %>% select(Subject, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,4:99], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,100:195], na.rm = T)
p1m <- pro_1l %>% select(Subject, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,4:99], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,100:195], na.rm = T)
p1h <- pro_1l %>% select(Subject, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

pd <- pd %>% dplyr::rename(sub = Subject)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "verbal" & processing == "spatial")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- dvv

###
pro_1 <- read_excel(paste0(pth, "Vergauwe et al. 2012.xlsx"),
                    sheet = "Exp2_processing performance", range = "A1:CD1297")
pro_1l <- subset(pro_1, Procedure == "blockprocL")
pro_1l$acc <- rowMeans(pro_1l[,3:42], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,43:82], na.rm = T)
p1l <- pro_1l %>% select(Subject, acc, rt)
p1l$CL <- "low"

pro_1l <- subset(pro_1, Procedure == "blockprocM")
pro_1l$acc <- rowMeans(pro_1l[,3:42], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,43:82], na.rm = T)
p1m <- pro_1l %>% select(Subject, acc, rt)
p1m$CL <- "medium"

pro_1l <- subset(pro_1, Procedure == "blockprocH")
pro_1l$acc <- rowMeans(pro_1l[,3:42], na.rm = T)
pro_1l$rt <- rowMeans(pro_1l[,43:82], na.rm = T)
p1h <- pro_1l %>% select(Subject, acc, rt)
p1h$CL <- "high"

pl <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1l, FUN = mean)
pm <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1m, FUN = mean)
ph <- aggregate(cbind(acc,rt) ~ Subject+CL, data = p1h, FUN = mean)
pd <- rbind(pl,pm,ph)

pd <- pd %>% dplyr::rename(sub = Subject)

id_vec <- sort(unique(vergauwe10$sub))
cls <- unique(levels(vergauwe10$CL))
dvv <- subset(vergauwe10, memory == "spatial" & processing == "verbal")
for (subj in id_vec) {
  for (cl in cls) {
    dvv$acc[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$acc[pd$sub == subj & pd$CL == cl]
    dvv$rt[dvv$sub == subj & levels(dvv$CL) == cl] <- pd$rt[pd$sub == subj & pd$CL == cl]
  }
}

verg10 <- rbind(verg10,dvv)

vergauwe10 <- verg10

vergauwe10 <- vergauwe10 %>% dplyr::rename(meanAcc = acc, meanRT = rt)
vergauwe10 <- vergauwe10 %>% dplyr::rename(subj = sub)
vergauwe10$totaltime <- 0
vergauwe10$totaltime[vergauwe10$CL == "low"] <- 2000
vergauwe10$totaltime[vergauwe10$CL == "medium"] <- 1239
vergauwe10$totaltime[vergauwe10$CL == "high"] <- 1000

vergauwe10$cogload <- vergauwe10$meanRT/vergauwe10$totaltime

aggregate(cbind(span, cogload) ~ CL + memory + processing, data = vergauwe10, FUN = mean)

vergauwe10$exp[vergauwe12$memory == "verbal"] <- 1
vergauwe10$exp[vergauwe12$memory == "spatial"] <- 2

vergauwe12 <- vergauwe10 %>% select(exp, subj, memory, processing, CL, totaltime, meanRT,
                                    meanAcc, cogload, span)

save(vergauwe12, file = "./pkg/data/vergauwe12.rda")


pd <- aggregate(cbind(span, cogload) ~  CL + memory + processing, data = vergauwe12, FUN = mean)

plot(c(0.2,0.7), c(1.5,6.0), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.2,0.3,0.4,0.5,0.6,0.7), labels = T)
points(x = pd$cogload[pd$memory == "verbal"],
       y = pd$span[pd$memory == "verbal"],
       type = "p", pch = 15, col = "red")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal"),])$coefficients,
       lty = 1, pch = 15, col = "red")

points(x = pd$cogload[pd$memory == "spatial"],
       y = pd$span[pd$memory == "spatial"],
       type = "p", pch = 17, col = "green")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial"),])$coefficients,
       lty = 3, pch = 17, col = "green")

legend(0.2, 1.5, c("verbal-spatial",
                   "spatial-verbal"), lty = c(1,3),
       pch=c(15,17), col = c("red","green"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)


####################################################$
### Vergauwe 15
rm(list = ls())
pth <- "BenchmarksWM.Data/BM5.2.4.CogLoad/"
ver15 <- read_excel(paste0(pth, "Vergauwe et al. 2015 behavioral.xlsx"),
                    sheet = "Feuil1", range = "A1:AZ1729")


ver15 <- ver15 %>% rename(subj = sub, domain = `Running[Block]`, trial = Trial,
                          condition = CONDITION, num = Numbitems, probe = POSPROBE)

### compute Means

verb <- subset(ver15, domain == "VERBBLOCKLIST")
spat <- subset(ver15, domain == "SPATBLOCKLIST")

verb$proc.pc <- rowMeans(verb[,7:18], na.rm = T)
verb$proc.meanRT <- rowMeans(verb[,28:39], na.rm = T)
spat$proc.pc <- rowMeans(spat[,19:27], na.rm = T)
spat$proc.meanRT <- rowMeans(spat[,40:48], na.rm = T)

verb$domain <- "verbal"
spat$domain <- "spatial"
verb$condition[verb$condition == "3EASY"] <- "easy"
verb$condition[verb$condition == "9EASY"] <- "easy"
verb$condition[verb$condition == "3HARD"] <- "hard"
verb$condition[verb$condition == "9HARD"] <- "hard"

spat$condition[spat$condition == "3EASY"] <- "easy"
spat$condition[spat$condition == "6EASY"] <- "easy"
spat$condition[spat$condition == "3HARD"] <- "hard"
spat$condition[spat$condition == "6HARD"] <- "hard"

verb <- verb %>% select(subj, trial, domain, condition, num, probe, proc.pc, proc.meanRT,
                        RECALLV.ACC, RECALLV.RT)
verb <- verb %>% rename(acc = RECALLV.ACC, rt = RECALLV.RT)

spat <- spat %>% select(subj, trial, domain, condition, num, probe, proc.pc, proc.meanRT,
                        RECALLS.ACC, RECALLS.RT)
spat <- spat %>% rename(acc = RECALLS.ACC, rt = RECALLS.RT)

vergauwe15 <- rbind(verb, spat)

vergauwe15$domain <- as.factor(vergauwe15$domain)
vergauwe15$condition <- as.factor(vergauwe15$condition)
vergauwe15$num <- as.numeric(vergauwe15$num)

vergauwe15$totaltime <- 9000

vergauwe15$CL <- vergauwe15$proc.meanRT/(vergauwe15$totaltime/as.numeric(vergauwe15$num))



save(vergauwe15, file = "./pkg/data/vergauwe15.rda")


pd <- aggregate(cbind(acc, CL) ~  domain + num, data = vergauwe15[which(vergauwe15$condition == "easy"),], FUN = mean)

plot(c(0.05,0.25), c(0.5,1), type = "n", xlab = "Cognitive Load (Mean RT / Total Time)",
     ylab = "Proportion correct", main = "Easy Processing Task", xaxt = "n")
axis(side = 1, at = c(0.05,0.1,0.15,0.2,0.25), labels = T)
points(x = pd$CL[pd$domain == "verbal"],
       y = pd$acc[pd$domain == "verbal"],
       type = "p", pch = 15, col = "red")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "verbal"),])$coefficients,
       lty = 1, pch = 15, col = "red")

points(x = pd$CL[pd$domain == "spatial"],
       y = pd$acc[pd$domain == "spatial"],
       type = "p", pch = 17, col = "green")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "spatial"),])$coefficients,
       lty = 3, pch = 17, col = "green")

legend(0.05, 0.5, c("verbal-many",
                   "spatial-many"), lty = c(1,3),
       pch=c(15,17), col = c("red","green"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)

pd <- aggregate(cbind(acc, CL) ~  domain + num, data = vergauwe15[which(vergauwe15$condition == "hard"),], FUN = mean)

plot(c(0.05,0.25), c(0.5,1), type = "n", xlab = "Cognitive Load (Mean RT / Total Time)",
     ylab = "Proportion correct", main = "Hard Processing Task", xaxt = "n")
axis(side = 1, at = c(0.05,0.1,0.15,0.2,0.25), labels = T)
points(x = pd$CL[pd$domain == "verbal"],
       y = pd$acc[pd$domain == "verbal"],
       type = "p", pch = 15, col = "red")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "verbal"),])$coefficients,
       lty = 1, pch = 15, col = "red")

points(x = pd$CL[pd$domain == "spatial"],
       y = pd$acc[pd$domain == "spatial"],
       type = "p", pch = 17, col = "green")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "spatial"),])$coefficients,
       lty = 3, pch = 17, col = "green")

legend(0.05, 0.5, c("verbal-many",
                    "spatial-many"), lty = c(1,3),
       pch=c(15,17), col = c("red","green"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)


