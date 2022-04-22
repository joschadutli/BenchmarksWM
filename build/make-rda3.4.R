## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM3.4 #####################
pth  <- "BenchmarksWM.Data/BM3.4.1.OutputOrder/"

## I guess Cowan 2002.

fnam <- paste0(pth, "Cowan2002JML.xls")
# Load data
data = read_excel(fnam, sheet="Raw data with RTs", range="A1:AD3841")

# Modality = visual (V) or auditory (A)
# StimGroup = grouped (G) or ungrouped (U)
# Part/Whole = partial recall (P) or whole recall (W)
# StartPos = start position of recall: 1, 4, or 7
# TBRs = to-be-remembered stimuli
# Response = reported stimuli (... for list items not to be reported in partial recall)
# i1 to i9: accuracy of recall in list positions 1:9
# rt1 to rt9: cumulative response times for recalling items in list positions 1:9;
# these times start with the StartPos (so for instance, when StartPos=7, rt7 is the shortest time because that is the list position the person started at)
data <- data %>% rename(subj = Subject) %>% rename(trial = `Trial#`) %>%
  rename(part = `Part/Whole`)

d_acc <- gather(data = data, key = "serpos", value = "corr", 12:20)

d_rt <- gather(data = data, key = "serpos", value = "rt", 21:29)

d <- d_acc %>% select(subj, trial, Modality, StimGroup, StartPos, part, serpos, corr)
d <- cbind(d, d_rt$rt, d_rt$`invalid RT =1`, d_rt$trial)
check <- unique(d$trial == d$`d_rt$trial`)
d <- d %>% rename(rt = `d_rt$rt`)
names(d)[which(names(d) == "d_rt$`invalid RT =1`")] <- "inval"


d$rt[d$inval == 1] <- NA
d$rt[d$rt == "."] <- NA
d$corr[d$corr == "."] <- NA
d$corr <- as.numeric(d$corr)
d$rt <- as.numeric(d$rt)

for (sp in 1:9) {
  ind <- as.character(sp)
  selSP <- grepl(ind, d$serpos, fixed = T)
  d$serpos[selSP] <- sp
}


cowan02 <- d %>% select(subj, trial, Modality, StimGroup, StartPos, part, serpos, corr, rt)
save(cowan02, file="./pkg/data/cowan02.rda", compress = "xz")

u <- cowan02[which(cowan02$StimGroup == "U"),]
uv <- u[which(u$Modality == "V"),]

pd <- aggregate(corr ~ StartPos*serpos, data = uv, FUN = mean)
bgk1 <- c("black","", "", "grey40", "","", "white")
bgk <- c("black","grey40", "white")

maxx <- 9
miny <- 0.0
plot(c(1,9),c(0,1), xlim=c(0, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1, cex.axis=1,
     main = "Ungrouped, visually pres. digits")
for (sp in c(1,4,7)) {
  pid <- subset(pd, StartPos==sp)
  lines(x = pid$serpos,
        y = pid$corr,
        type = "l", lty = 1)
  points(x = pid$serpos,
         y = pid$corr,
         pch = 21, bg = bgk1[sp])
}
legend(0,miny,c("N=1", "N=4", "N=7"), pch=21, pt.bg = bgk, xjust=0, yjust=0,
       title = "Start Pos", cex = 0.8)



### BM3.4.2 Lange et al. (2011)
pth <- "BenchmarksWM.Data/BM3.4.3.OutputContiguity/"

# Read Data ---------------------------------------------------------------------------------------------------
#for Exp.1
E1_data <- read.csv(paste0(pth, "Exp1_data.csv"), header=T, sep=",")
E2_data <- read.csv(paste0(pth, "Exp2_data.csv"), header=T, sep=",")
E4_data <- read.csv(paste0(pth, "Exp4_data.csv"), header=T, sep=",")

### prepare d1 from Exp1
d1 <- E1_data %>% select(id, memo, con, Exp, nblock, trial, n, out_nr, out_pos,
                         example,
                         neg, pos, corr, RT, RT_r)
d1 <- d1 %>% rename(task = memo) %>% rename(exp = Exp) %>% rename(outnr = out_nr) %>%
  rename(outpos = out_pos) %>% rename(ptype = neg) %>% rename(rt_raw = RT) %>%
  rename(rt = RT_r) %>% rename(n_block = nblock) %>% rename(inpos = example) %>%
  rename(size = n) %>% rename(magn = pos)
d1$inpos <- NA
d1$magn[d1$task == 1] <- NA

### prepare d2 from Exp 2 (has an additional column: inpos)
d2 <- E2_data %>% select(id, memo, con, Exp, nblock, trial, n, outnr, in_nr, in_pos,
                         neg, pos, corr, RT, RT_r)
d2 <- d2 %>% rename(task = memo) %>% rename(exp = Exp) %>% rename(outpos = in_nr) %>%
  rename(ptype = neg) %>% rename(rt_raw = RT) %>% rename(rt = RT_r) %>%
  rename(n_block = nblock) %>% rename(inpos = in_pos) %>% rename(size = n) %>%
  rename(magn = pos)
d2$magn[d2$task == 1] <- NA

### prepare d4 from Exp 4
d4 <- E4_data %>% select(id, memo, con, Exp, nblock, trial, n, outnr, out_pos,
                         example,
                         neg, pos, corr, RT, RT_r)
d4 <- d4 %>% rename(task = memo) %>% rename(exp = Exp) %>%
  rename(n_block = nblock) %>% rename(outpos = out_pos) %>% rename(inpos = example) %>%
  rename(ptype = neg) %>% rename(rt_raw = RT) %>% rename(rt = RT_r) %>%
  rename(size = n) %>% rename(magn = pos)
d4$inpos <- NA
d4$magn[d4$task == 1] <- NA

d12 <- rbind(d1, d2)
d <- rbind(d12, d4)

lange11 <- d
save(lange11, file = "./pkg/data/lange11.rda", compress = "xz")

## a plot:

plotd <- lange11[which(lange11$exp == 1),] #experiment 1
taskd <- plotd[which(plotd$task == 1),] #recognition memory data only
pd_acc <- aggregate(corr ~ size*con, data = taskd, FUN = mean)
pd_rt <- aggregate(rt ~ size*con, data = taskd, FUN = mean)

#plotting
bgk <- c("black","grey40", "grey80", "white")
pt <- 21:24

maxx <- 5
miny <- 500
plot(c(3,5),c(500,1200), xlim=c(3, maxx),ylim=c(miny,1200), type="n", las=1,
     xlab="Set Size", ylab="Proportion Correct",cex.lab=1, cex.axis=1,
     main = "Response Times by Output-Order Condition
     and Set Size")
for (sp in 1:4) {
  pid <- subset(pd_rt, con==sp)
  lines(x = pid$size,
        y = pid$rt,
        type = "l", lty = 1)
  points(x = pid$size,
         y = pid$rt,
         pch = pt[sp], bg = bgk[sp])
}
legend(3,1200,c("forward", "fixed-irr.", "backward", "random"),
       pch=pt, pt.bg = bgk, xjust=0, yjust=1,
       title = "Order Conditions", cex = 0.6)

### accuracy
maxx <- 5
miny <- 0.9
plot(c(3,5),c(0.4,1), xlim=c(3, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Set Size", ylab="Proportion Correct",cex.lab=1, cex.axis=1,
     main = "Accuracy by Output-Order Condition")
for (sp in 1:4) {
  pid <- subset(pd_acc, con==sp)
  lines(x = pid$size,
        y = pid$corr,
        type = "l", lty = 1)
  points(x = pid$size,
         y = pid$corr,
         pch = pt[sp], bg = bgk[sp])
}
legend(3,miny,c("forward", "fixed-irr.", "backward", "random"),
       pch=pt, pt.bg = bgk, xjust=0, yjust=0,
       title = "Order Conditions", cex = 0.6)


### Grenfell-Essam & Ward (2012)
#read only the sheet as a separate data frame
pth <- "BenchmarksWM.Data/BM3.5.1.FirstRecallProb/"
gew <- read_excel(paste0(pth, "GrenfellEssamWard4figure.xlsx"),sheet=1, col_names=F)
names(gew) <- c("cueing","strategy",paste("LL",c(2,	4,	5,	6,	7,	8,	12,	15),sep=""))

#gather and extract and so on
gew2<-  gather(gew,"List Length","Probability of First Recall",starts_with("LL"))
gew2[[3]] <- as.numeric(substr(gew2[[3]],3,5))

gew2 <- gew2 %>% rename(listlength = `List Length`) %>% rename(prob_fr = `Probability of First Recall`) %>% rename(cue = cueing) %>% rename(output = strategy)

gew2$cue[gew2$cue == "Post-cued IFR"] <- "post-presentation"
gew2$cue[gew2$cue == "Pre-cued IFR"] <- "pre-presentation"

grenfell12 <- gew2
save(grenfell12, file="./pkg/data/grenfell12.rda", compress = "xz")

post <- filter(grenfell12, cue=="post-presentation")
pre <- filter(grenfell12, cue=="pre-presentation")

colors <- c("black", "grey50", "grey80", "white")

## written by K. Oberauer
plotpanel <- function(post,title4panel) {
  plot(0,0, xlim=c(0,max(post$listlength)+1),ylim=c(0,1), type="n", las=1, xaxt="n",
       xlab="List Length", ylab="Probability of First Recall",cex.lab=0.8)
  k<-0
  for (rx in unique(post$output)) {
    tbp<-filter(post, output==rx)
    k<-k+1
    lines(tbp$listlength, tbp$prob_fr,lwd=1,lty=k)
    points(tbp$listlength, tbp$prob_fr,pch=20+k, bg=colors[k],cex=0.8)
  }
  text(7.5,.95,title4panel,cex=1.0)
  legend(10,.6,unique(post$output),lty=c(1:k),pch=c(21:(20+k)),pt.bg=colors,pt.cex=0.6,y.intersp = 1, cex = 0.6)
  par(tcl= -0.2)  #minor ticks
  axis(1, at=seq(from=0,to=max(post$listlength)+1,by=1), labels=F, lwd=1, lwd.ticks=0.5)
  par(tcl= -0.5)  #major ticks with labels
  axis(1, at=seq(from=0,to=max(post$listlength)+1,by=3), labels=seq(from=0,to=max(post$listlength)+1,by=3),
       lwd=0, lwd.ticks=1)
}


par(mfrow=c(1,2),mar=c(4, 4, 2, 1) + 0.2)
plotpanel(pre,"Pre-cued")
plotpanel(post,"Post-cued")


### Healey & Kahana (2014) on the clustering of semantically similar pairs of
### words in free recall.

rm(list=ls())

pth <- "BenchmarksWM.Data/BM3.5.2.SemanticClustering/"
data <- read.table(paste0(pth, "Healy14.dat"), header=F)
names(data) <- c("subject", "session",
                 "word1", "word2", "word3", "word4", "word5", "word6", "word7", "word8",
                 "word9", "word10", "word11", "word12", "word13", "word14", "word15", "word16",
                 "recword1", "recword2", "recword3", "recword4", "recword5", "recword6", "recword7",
                 "recword8", "recword9", "recword10", "recword11", "recword12", "recword13", "recword14",
                 "recword15", "recword16", "recword17", "recword18", "recword19", "recword20", "recword21",
                 "recword22", "recword23", "recword24", "recword25", "recword26", "recword27", "recword28",
                 "recinpos1", "recinpos2", "recinpos3", "recinpos4", "recinpos5", "recinpos6", "recinpos7",
                 "recinpos8", "recinpos9", "recinpos10", "recinpos11", "recinpos12", "recinpos13", "recinpos14",
                 "recinpos15", "recinpos16", "recinpos17", "recinpos18", "recinpos19", "recinpos20", "recinpos21",
                 "recinpos22", "recinpos23", "recinpos24", "recinpos25", "recinpos26", "recinpos27", "recinpos28",
                 "rectime1", "rectime2", "rectime3", "rectime4", "rectime5", "rectime6", "rectime7",
                 "rectime8", "rectime9", "rectime10", "rectime11", "rectime12", "rectime13", "rectime14",
                 "rectime15", "rectime16", "rectime17", "rectime18", "rectime19", "rectime20", "rectime21",
                 "rectime22", "rectime23", "rectime24", "rectime25", "rectime26", "rectime27", "rectime28")

# wordX: identification number of word presented in list position X
# recwordY: identification number of word recalled in output position Y
# recinposY: list position of word recalled in output position Y
# rectimeY: cumulative response time of word recalled in output position Y

## I don't get it, so I just read it in as it is:
healey14 <- data

### all Andy's fix code...
subject <- healey14$subject[1]
session <- healey14$session[1]
listN <- 0
healey14$listN <- 0
for(rw in 1:nrow(healey14)) {
  if(subject == healey14$subject[rw] & session == healey14$session[rw]) {
    listN <- listN + 1
  } else {
    print(rw)
    listN <- 1
    subject <- healey14$subject[rw]
    session <- healey14$session[rw]
  }
  healey14$listN[rw] <- listN
}

## Dataset has four distinct components.
## Split into these, and pivot each to long format

word <- healey14 %>% select(subject:word16, listN) %>% add_column(type = "word")

wordL <- word %>% pivot_longer(word1:word16,
                               names_to = "pos",
                               names_prefix = "word",
                               values_to = "value")

recword <- healey14 %>% select(subject:session, recword1:recword28, listN) %>%
  add_column(type = "recword")

recwordL <- recword %>% pivot_longer(recword1:recword28,
                                     names_to = "pos",
                                     names_prefix = "recword",
                                     values_to = "value")

recinpos <- healey14 %>% select(subject:session, recinpos1:recinpos28, listN) %>%
  add_column(type = "recinpos")

recinposL <- recinpos %>% pivot_longer(recinpos1:recinpos28,
                                       names_to = "pos",
                                       names_prefix = "recinpos",
                                       values_to = "value")

rectime <- healey14 %>% select(subject:session, rectime1:rectime28, listN) %>%
  add_column(type = "rectime")

rectimeL <- rectime %>% pivot_longer(rectime1:rectime28,
                                     names_to = "pos",
                                     names_prefix = "rectime",
                                     values_to = "value")

## Combine back into one long-format dataset

healey14 <- bind_rows(wordL, recwordL, recinposL, rectimeL)

## Fix data types
healey14$pos <- as.integer(healey14$pos)
healey14$listN <- as.integer(healey14$listN)

## The four types of data are actually in two categories - one about presented
## words, and three about the recall of words. For human readability, it is
## useful to classify and order in this way
healey14$inout <- "output"
healey14$inout[healey14$type == "word"] <- "input"
healey14 <- healey14 %>% arrange(subject, session, listN, inout, pos, type)

## inout was useful for ordering but adds redundancy to dataset. Remove
healey14 <- healey14 %>% select(-inout)

## The matrix representation of the dataset was not particularly suited to it
## because they number of recalled items varied by list.
## Now it's in long-format, we can cut out the unused rows
healey14 <- healey14 %>% filter(value != 0)

## Finally, the package is based on dataframes rather than tibbles, so
## convert to df and save
healey14 <- as.data.frame(healey14)
save(healey14, file="./pkg/data/healey14.rda", compress = "xz")

## subset data and make it long:

d_pres <- data[1:18]
dpr <- gather(d_pres, key = "serpos_bad", value = "word", 3:18)

for (i in rev(seq(1:16))) {
  ind <- as.character(i)
  selOut <- grepl(ind, dpr$serpos_bad, fixed = TRUE)
  dpr$serpos[selOut] <- as.numeric(i)
  dpr$serpos_bad[selOut] <- NA
}
# check
dpr <- dpr %>% select(subject, session, serpos, word)

d_words <- data[c(1:2, 19:46)]
dwr <- gather(d_words, key = "outpos_bad", value = "word", 3:30)
for (i in rev(seq(1:28))) {
  ind <- as.character(i)
  selOut <- grepl(ind, dwr$outpos_bad, fixed = TRUE)
  dwr$outpos[selOut] <- as.numeric(i)
  dwr$outpos_bad[selOut] <- NA
}
#check
unique(dwr$outpos)

dwr <- dwr %>% select(subject, session, outpos, word)

#check output position

unique(dwr$word[as.numeric(dwr$outpos) > 25 & dwr$word != 0])







