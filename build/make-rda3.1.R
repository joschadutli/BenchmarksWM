## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM2.1 #####################
pth  <- "BenchmarksWM.Data/BM3.1.SP.Accuracy/"

## forward and backward serial recall (Madigan, 1971)
fnam <- paste0(pth, "Madigan.1971.txt")

madigan <- read.table(fnam, colClasses=c("character"), header=F)
M <- as.data.frame(matrix(0, dim(madigan)[1], 11))
for (line in 1:dim(madigan)[1]) {
  M[line,1] <- as.numeric(substring(madigan[line,], 1,2))
  for (col in 2:11) {
    M[line,col] <- as.numeric(substring(madigan[line,], first=col+1, last=col+1))
  }
}
names(M) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8")
# 1 = audio, 2 = visual; 1 = forward, 2 = backward
accuracy <- t(apply(M[,4:11], MARGIN=1, FUN=function(x){as.numeric(as.numeric(x)==c(1:8))}))
Macc <- cbind(M[,1:3], accuracy)
names(Macc) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8") 
Magg <- aggregate(cbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8) ~ id+modality+direction, data=Macc, FUN=mean)
names(Magg) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8") 

#to long format:

d <- gather(Magg, key = "serpos", value = "acc", 4:11)

d$serpos[d$serpos == "sp1"] <- 1
d$serpos[d$serpos == "sp2"] <- 2
d$serpos[d$serpos == "sp3"] <- 3
d$serpos[d$serpos == "sp4"] <- 4
d$serpos[d$serpos == "sp5"] <- 5
d$serpos[d$serpos == "sp6"] <- 6
d$serpos[d$serpos == "sp7"] <- 7
d$serpos[d$serpos == "sp8"] <- 8

madigan71 <- d
save(madigan71, file="./pkg/data/madigan71.rda")

# Reproduce Figure 7a in Obverauer et al. (2018) (data for visual presentation modality)
visual <- madigan71[which(magidan71$modality == 2),]

pd <- aggregate(acc ~ serpos*direction, data = visual, FUN = mean)

par(mfrow=c(1,2))
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", 
     main = "Serial Recall", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = T)

lines(x = pd$serpos[pd$direction == 1], 
      y = pd$acc[pd$direction == 1], 
      type = "l", lty = 1)
lines(x = pd$serpos[pd$direction == 2], 
      y = pd$acc[pd$direction == 2], 
      type = "l", lty = 2)
legend(1, 1.0, c("Forward", "Backward"), lty = 1:2,
       horiz = F, cex = 0.5, yjust = 1, xjust = 0)


##################################
### Peteranderl & Oberauer (2017)

fnam <- paste0(pth, "Peteranderl.2017.long.dat")

colordata <- read.table(fnam, header=F)
names(colordata)= c("id", "session", "trial", "serpos", 
                    "target", "response", "iti1", "iti2", "iti3", "iti4", "time")

colordata$AS[colordata$session == 1] <- 0
colordata$AS[colordata$session == 2] <- 1

colordata$targetrad <- pi*colordata$target/180
colordata$responserad <- pi*colordata$response/180
# Paul Bays' wrap function -> signed angular difference (radians)!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}
colordata$diffrad <- wrap(colordata$responserad - colordata$targetrad)
colordata$errorrad <- abs(colordata$diffrad)
colordata$errordeg <- 180*colordata$errorrad/pi
d <- aggregate(errordeg ~ id + trial + AS + serpos, data = colordata, FUN=mean)

peteranderl17 <- d
save(peteranderl17, file="./pkg/data/peteranderl17.rda")

pd <- aggregate(errordeg ~ AS*serpos, data = peteranderl17, FUN=mean)

par(mfrow=c(1,1))
plot(c(1,5), c(0.0,65), type = "n", xlab = "Serial Position",
     ylab = "Precision error in degrees", 
     main = "Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5), labels = T)

lines(x = pd$serpos[pd$AS == 0], 
      y = pd$errordeg[pd$AS == 0], 
      type = "l", lty = 1)
lines(x = pd$serpos[pd$AS == 1], 
      y = pd$errordeg[pd$AS == 1], 
      type = "l", lty = 2)
legend(1, 65, c("silent", "AS"), lty = 1:2,
       horiz = F, cex = 0.5, yjust = 1, xjust = 0)

data("peteranderl17")
aggdata <- aggregate(errordeg ~ id + AS + serpos, data = peteranderl17, FUN=mean)

lineplot.ci(data=aggdata, dv="errordeg", iv=c("serpos", "AS"), id=1, x=1, off=0.05, ylim=c(0,80),
             cex=1.3, lty=1, pt = 21:22, ptcol=c("black","grey"), xlab="Serial Position", ylab="Error (deg)")
legend(4,0,c("Silent", "AS"), pt.bg=c("black","grey"), lty=c(1,1), pch=21:22, pt.cex = 1.5, yjust=0)


##########################
### Oberauer (2003b)

fnam1 <- paste0(pth, "Oberauer.2003.GlobRec.dat")
fnam2 <- paste0(pth, "Oberauer.2003.LocRec.dat")

globrec <- read.table(fnam1, header=T)
locrec <- read.table(fnam2, header=TRUE, dec=".")

### ask Prof. Oberauer about variable names. 
### Probably best way to transform is to subset each section (e.g. 2:7)
### and transform to long format, than either rbind() or cbind()

# Zu den Datensätzen GlobRec und LocRec: Die „r“ stehen für „Reaktionszeit“
# und die „w“ für „Wahrheitswert“ (also Korrektheit). Die Zahl 1 steht für
# „random order of recall“; 2 steht für „forward order of recall“.

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
ptypes <- c(21:25, 21:25)


# global.PCpos.inpos.random <- globrec[,2:7]
# global.PCpos.inpos.forward <- globrec[,14:19]
# local.PCpos.inpos.random <- locrec[,2:7]
# local.PCpos.inpos.forward <- locrec[,14:19]

loc_input_random <- locrec[,1:13]
loc_input_forward <- locrec[, c(1, 14:25)]
loc_spatialpos_random <- locrec[, c(1,26:37)]
loc_spatialpos_forward <- locrec[c(1,38:49)]
loc_output_random <- locrec[,c(1,50:61)]
loc_output_forward <- locrec[c(1,62:73)]

lif_acc <- loc_input_forward[,1:7]
lif_accl <- gather(data = lif_acc, key = "inpos", value = "acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lif_accl$inpos,5,5), fixed = TRUE)
  lif_accl$inpos[selOut] <- as.numeric(i)
}  

lsf_acc <- loc_spatialpos_forward[,1:7]
lsf_accl <- gather(data = lsf_acc, key = "spatialpos", value = "acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lsf_accl$spatialpos,5,5), fixed = TRUE)
  lsf_accl$spatialpos[selOut] <- as.numeric(i)
}  

lof_acc <- loc_output_forward[,1:7]
lof_accl <- gather(data = lof_acc, key = "outpos", value = "acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lof_accl$outpos,6,6), fixed = TRUE)
  lof_accl$outpos[selOut] <- as.numeric(i)
}  

loc_acc <- cbind(lif_accl, lsf_accl$spatialpos, lof_accl$outpos)

names(loc_acc)[which(names(loc_acc) == "lsf_accl$spatialpos")] <- "spatialpos"
names(loc_acc)[which(names(loc_acc) == "lof_accl$outpos")] <- "outpos"

loc_rt_forward <- loc_input_forward[,c(1,8:13)]
lf_rt <- gather(loc_rt_forward, key = "inpos", value = "rt", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lf_rt$inpos,5,5), fixed = TRUE)
  lf_rt$inpos[selOut] <- as.numeric(i)
}

lf_rt$rt <- as.numeric(lf_rt$rt)

loc_forward <- cbind(loc_acc, lf_rt$rt)
names(loc_forward)[which(names(loc_forward) == "lf_rt$rt")] <- "rt"

loc_forward$cond <- "forward"
loc_forward$memory <- "local"

loc_forward <- loc_forward %>% rename(serpos = inpos) 
loc_forward <- loc_forward %>% rename(in_acc = acc) %>%
  rename(in_rt = rt)
loc_forward$out_acc <- loc_forward$in_acc
loc_forward$out_rt <- loc_forward$in_rt
loc_forward$spatial_acc <- loc_forward$in_acc
loc_forward$spatial_rt <- loc_forward$in_rt
loc_forward <- loc_forward %>% rename(subj = vp)

loc_forward <- loc_forward %>% select(subj, memory, cond, serpos, in_acc, 
                                      spatial_acc, out_acc, in_rt, spatial_rt,
                                      out_rt)

### now do the random part in loc.
# compare values within particicipants in a forloop with unique(sort())
# then create new variable which is output for example and let this
# be the value of the previously gathered output dataset.
# hope that within a participant there are no exactly-equal values.

lir_acc <- loc_input_random[,1:7]
lir_accl <- gather(lir_acc, key = "serpos", value = "in_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lir_accl$serpos,5,5), fixed = TRUE)
  lir_accl$serpos[selOut] <- as.numeric(i)
}

lsr_acc <- loc_spatialpos_random[,1:7]
lsr_accl <- gather(lsr_acc, key = "serpos", value = "spatial_acc", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lsr_accl$serpos,5,5), fixed = TRUE)
  lsr_accl$serpos[selOut] <- as.numeric(i)
}

lor_acc <- loc_output_random[,1:7]
lor_accl <- gather(lor_acc, key = "serpos", value = "out_acc", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lor_accl$serpos,6,6), fixed = TRUE)
  lor_accl$serpos[selOut] <- as.numeric(i)
}

lir_rt <- loc_input_random[,c(1,8:13)]
lir_rt_l <- gather(lir_rt, key = "serpos", value = "in_rt", 2:7) 
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lir_rt_l$serpos,5,5), fixed = TRUE)
  lir_rt_l$serpos[selOut] <- as.numeric(i)
}

lsr_rt <- loc_spatialpos_random[,c(1,8:13)]
lsr_rt_l <- gather(lsr_rt, key = "serpos", value = "spatial_rt", 2:7) 
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lsr_rt_l$serpos,5,5), fixed = TRUE)
  lsr_rt_l$serpos[selOut] <- as.numeric(i)
}

lor_rt <- loc_output_random[,c(1,8:13)]
lor_rt_l <- gather(lor_rt, key = "serpos", value = "out_rt", 2:7) 
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(lor_rt_l$serpos,6,6), fixed = TRUE)
  lor_rt_l$serpos[selOut] <- as.numeric(i)
}

lir <- cbind(lir_accl, lir_rt_l$in_rt, lsr_accl$spatial_acc, lsr_rt_l$spatial_rt,
             lor_accl$out_acc, lor_rt_l$out_rt)

names(lir)[which(names(lir) == "lir_rt_l$in_rt")] = "in_rt"
names(lir)[which(names(lir) == "lsr_rt_l$spatial_rt")] = "spatial_rt"
names(lir)[which(names(lir) == "lor_rt_l$out_rt")] = "out_rt"
names(lir)[which(names(lir) == "lsr_accl$spatial_acc")] = "spatial_acc"
names(lir)[which(names(lir) == "lor_accl$out_acc")] = "out_acc"

lir <- lir %>% rename(subj = vp)
lir$memory <- "local"
lir$cond <- "random"

loc_random <- lir %>% select(subj, memory, cond, serpos, in_acc, 
                             spatial_acc, out_acc, in_rt, spatial_rt,
                             out_rt)
loc_long <- rbind(loc_forward, loc_random)

##############
### global recognition memory:
glob_input_random <- globrec[,1:13]
glob_input_forward <- globrec[, c(1, 14:25)]
glob_spatialpos_random <- globrec[, c(1,26:37)]
glob_spatialpos_forward <- globrec[c(1,38:49)]
glob_output_random <- globrec[,c(1,50:61)]
glob_output_forward <- globrec[c(1,62:73)]

gif_acc <- glob_input_forward[,1:7]
gif_accl <- gather(data = gif_acc, key = "serpos", value = "in_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gif_accl$serpos,5,5), fixed = TRUE)
  gif_accl$serpos[selOut] <- as.numeric(i)
}  

gsf_acc <- glob_spatialpos_forward[,1:7]
gsf_accl <- gather(data = gsf_acc, key = "serpos", value = "spatial_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gsf_accl$serpos,5,5), fixed = TRUE)
  gsf_accl$serpos[selOut] <- as.numeric(i)
}  

gof_acc <- glob_output_forward[,1:7]
gof_accl <- gather(data = gof_acc, key = "serpos", value = "out_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gof_accl$serpos,6,6), fixed = TRUE)
  gof_accl$serpos[selOut] <- as.numeric(i)
}  

grf <- glob_input_forward[,c(1,8:13)]
grfl <- gather(grf, key = "serpos", value = "in_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(grfl$serpos,5,5), fixed = TRUE)
  grfl$serpos[selOut] <- as.numeric(i)
} 

gsrf <- glob_spatialpos_forward[,c(1,8:13)]
gsrfl <- gather(gsrf, key = "serpos", value = "spatial_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gsrfl$serpos,5,5), fixed = TRUE)
  gsrfl$serpos[selOut] <- as.numeric(i)
} 

gorf <- glob_output_forward[,c(1,8:13)]
gorfl <- gather(gorf, key = "serpos", value = "out_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gorfl$serpos,6,6), fixed = TRUE)
  gorfl$serpos[selOut] <- as.numeric(i)
} 

globl <- cbind(gif_accl, grfl$in_rt, gsf_accl$spatial_acc, gsrfl$spatial_rt,
               gof_accl$out_acc, gorfl$out_rt)

names(globl)[which(names(globl) == "grfl$in_rt")] = "in_rt"
names(globl)[which(names(globl) == "gsrfl$spatial_rt")] = "spatial_rt"
names(globl)[which(names(globl) == "gorfl$out_rt")] = "out_rt"
names(globl)[which(names(globl) == "gsf_accl$spatial_acc")] = "spatial_acc"
names(globl)[which(names(globl) == "gof_accl$out_acc")] = "out_acc"

globl <- globl %>% rename(subj = vp)
globl$memory <- "global"
globl$cond <- "forward"

glob_forward <- globl %>% select(subj, memory, cond, serpos, in_acc, 
                                 spatial_acc, out_acc, in_rt, spatial_rt,
                                 out_rt)

### global random

gir_acc <- glob_input_random[,1:7]
gir_accl <- gather(data = gir_acc, key = "serpos", value = "in_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gir_accl$serpos,5,5), fixed = TRUE)
  gir_accl$serpos[selOut] <- as.numeric(i)
}  

gsr_acc <- glob_spatialpos_random[,1:7]
gsr_accl <- gather(data = gsr_acc, key = "serpos", value = "spatial_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gsr_accl$serpos,5,5), fixed = TRUE)
  gsr_accl$serpos[selOut] <- as.numeric(i)
}  

gor_acc <- glob_output_random[,1:7]
gor_accl <- gather(data = gor_acc, key = "serpos", value = "out_acc", 2:7)

for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gor_accl$serpos,6,6), fixed = TRUE)
  gor_accl$serpos[selOut] <- as.numeric(i)
}  

grr <- glob_input_random[,c(1,8:13)]
grrl <- gather(grr, key = "serpos", value = "in_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(grrl$serpos,5,5), fixed = TRUE)
  grrl$serpos[selOut] <- as.numeric(i)
} 

gsrr <- glob_spatialpos_random[,c(1,8:13)]
gsrrl <- gather(gsrr, key = "serpos", value = "spatial_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gsrrl$serpos,5,5), fixed = TRUE)
  gsrrl$serpos[selOut] <- as.numeric(i)
} 

gorr <- glob_output_random[,c(1,8:13)]
gorrl <- gather(gorr, key = "serpos", value = "out_rt", 2:7)
for (i in 1:6) {
  ind <- as.character(i)
  selOut <- grepl(ind, substr(gorrl$serpos,5,5), fixed = TRUE)
  gorrl$serpos[selOut] <- as.numeric(i)
} 

globlr <- cbind(gir_accl, grrl$in_rt, gsr_accl$spatial_acc, gsrrl$spatial_rt,
               gor_accl$out_acc, gorrl$out_rt)

names(globlr)[which(names(globlr) == "grrl$in_rt")] = "in_rt"
names(globlr)[which(names(globlr) == "gsrrl$spatial_rt")] = "spatial_rt"
names(globlr)[which(names(globlr) == "gorrl$out_rt")] = "out_rt"
names(globlr)[which(names(globlr) == "gsr_accl$spatial_acc")] = "spatial_acc"
names(globlr)[which(names(globlr) == "gor_accl$out_acc")] = "out_acc"

globlr <- globlr %>% rename(subj = vp)
globlr$memory <- "global"
globlr$cond <- "random"

glob_random <- globlr %>% select(subj, memory, cond, serpos, in_acc, 
                                 spatial_acc, out_acc, in_rt, spatial_rt,
                                 out_rt)

glob_long <- rbind(glob_forward, glob_random)

oberauer03b <- rbind(loc_long, glob_long)
save(oberauer03b, file="./pkg/data/oberauer03b.rda")


## Reproduce Figure 7B in Oberauer et al. (2018)
pd <- oberauer03b[which(oberauer03b$cond == "random"),]
agg_pd <- aggregate(in_acc ~ serpos*memory, data = pd, FUN = mean)
plot(c(1,6), c(0.5,1.0), type = "n", xlab = "Serial Position (Input)",
     ylab = "Proportion correct", 
     main = "Serial Position Curve in Recognition Memory", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = T)

lines(x = agg_pd$serpos[agg_pd$memory == "global"], 
      y = agg_pd$in_acc[agg_pd$memory == "global"], 
      type = "l", lty = 1)
lines(x = agg_pd$serpos[agg_pd$memory == "local"], 
      y = agg_pd$in_acc[agg_pd$memory == "local"], 
      type = "l", lty = 1)
points(x = agg_pd$serpos[agg_pd$memory == "global"], 
       y = agg_pd$in_acc[agg_pd$memory == "global"],  
       pch = 21, bg = "black")
points(x = agg_pd$serpos[agg_pd$memory == "local"], 
       y = agg_pd$in_acc[agg_pd$memory == "local"],  
       pch = 22, bg = "grey")
legend(1.0, 1, c("Item Recognition", "Relational Recognition"), lty = 1, 
       pch = 21:22, pt.bg = c("black","grey"), horiz = F, cex = 0.8, yjust = 1, xjust = 0)












