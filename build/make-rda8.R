## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM8 Similarity #####################
pth  <- "BenchmarksWM.Data/BM8.1.PhonSimEffect/"


plotselection <- 2
# 1 = Plot just all-similar and all-dissimilar lists for Exp. 1, 2 = plot all conditions

dat1 <- read.table(paste0(pth,"Farrell.Lsky.normed1201.txt"),
                   col.names = c("subject",
                                 "trial",
                                 "scond",
                                 "c1","c2","c3","c4","c5","c6",
                                 "o.1","o.2","o.3","o.4","o.5","o.6",
                                 "rt1","rt2","rt3","rt4","rt5","rt6"))

# subject = subject number; trial = trial number, scond = similarity condition: 0 = all D, 1 = all S, 2 = D in Pos 2, 3 = D in Pos 4, 4 = D in Pos 6, 5 = SDSDSD
# c1 to c6: similar (0) or dissimilar (1) item in input positions 1 to 6
# o.1 to o.6: input position of the item reported in output positions 1 to 6; values > 6 are extralist intrusions; 0 = omissions. 
# rt1 to rt6: response times for outputs 1 to 6. 

dat2 <- read.table(paste0(pth,"psim2dat.ed"),
                   col.names = c("subject",
                                 "trial",
                                 "scond",
                                 "c1","c2","c3","c4","c5","c6",
                                 "o.1","o.2","o.3","o.4","o.5","o.6",
                                 "rt1","rt2","rt3","rt4","rt5","rt6"))

condd <- gather(dat1, key = "serpos", value = "similarity", c1:c6)
condd <- condd %>% select(subject, trial, scond, serpos, similarity)
condd$serpos <- as.numeric(substr(condd$serpos,2,2))

outd <- gather(dat1, key = "serpos", value = "output", o.1:o.6)
outd$serpos <-  as.numeric(substr(outd$serpos,3,3))

rtd <- gather(dat1, key = "serpos", value = "rt", rt1:rt6)
rtd$serpos <-  as.numeric(substr(rtd$serpos,3,3))

d1 <- cbind(condd, outd$output, rtd$rt)

names(d1)[names(d1) == "outd$output"] <- "output"
names(d1)[names(d1) == "rtd$rt"] <- "rt"

#similarity condition: 0 = all D, 1 = all S, 2 = D in Pos 2, 3 = D in Pos 4, 4 = D in Pos 6, 5 = SDSDSD

d1$condition[d1$scond == 0] <- "DDDDDD"
d1$condition[d1$scond == 1] <- "SSSSSS"
d1$condition[d1$scond == 2] <- "SDSSSS"
d1$condition[d1$scond == 3] <- "SSSDSS"
d1$condition[d1$scond == 4] <- "SSSSSD"
d1$condition[d1$scond == 5] <- "SDSDSD"

d1$acc <- as.numeric(d1$serpos==d1$output) 

far1 <- d1


condd <- gather(dat2, key = "serpos", value = "similarity", c1:c6)
condd <- condd %>% select(subject, trial, scond, serpos, similarity)
condd$serpos <- as.numeric(substr(condd$serpos,2,2))

outd <- gather(dat2, key = "serpos", value = "output", o.1:o.6)
outd$serpos <-  as.numeric(substr(outd$serpos,3,3))

rtd <- gather(dat2, key = "serpos", value = "rt", rt1:rt6)
rtd$serpos <-  as.numeric(substr(rtd$serpos,3,3))

d1 <- cbind(condd, outd$output, rtd$rt)

names(d1)[names(d1) == "outd$output"] <- "output"
names(d1)[names(d1) == "rtd$rt"] <- "rt"

#similarity condition: 0 = all D, 1 = all S, 2 = D in Pos 2, 3 = D in Pos 4, 4 = D in Pos 6, 5 = SDSDSD

d1$condition[d1$scond == 0] <- "DDDDDD"
d1$condition[d1$scond == 1] <- "SSSSSS"
d1$condition[d1$scond == 2] <- "SDSSSS"
d1$condition[d1$scond == 3] <- "SSSDSS"
d1$condition[d1$scond == 4] <- "SSSSSD"
d1$condition[d1$scond == 5] <- "SDSDSD"

d1$acc <- as.numeric(d1$serpos==d1$output) 


far2 <- d1

far1$exp <- 1
far2$exp <- 2

far1$task <- "serial recall"
far2$task <- "reconstruction"

farrell03 <- rbind(far1,far2)

farrell03 <- farrell03 %>% dplyr::rename(subj = subject, cond_num = scond)
farrell03 <- farrell03 %>% select(exp, subj, trial, task, condition,
                                  serpos, similarity, output, acc, rt)

farrell03$similarity[farrell03$similarity == 0] <- "S"
farrell03$similarity[farrell03$similarity == 1] <- "D"

save(farrell03, file="./pkg/data/farrell03.rda")

## Figure 16 upper panel

psd <- farrell03[which(farrell03$exp == 1),]
pd <- aggregate(acc ~ serpos+condition, data = psd, FUN = mean)
plot(c(1,6), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Phonological Similarity", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = levels(pd$serpos), 
     cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "DDDDDD"], 
      y = pd$acc[pd$condition == "DDDDDD"], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$serpos[pd$condition == "SSSSSS"], 
      y = pd$acc[pd$condition == "SSSSSS"], 
      type = "b", lty = 2, pch = 17, col = "grey")
legend(1, 0.0, c("dissimilar","similar"), lty = 1:2, 
       pch=c(15,17), col = c("black", "grey"), 
       horiz = F, cex = 0.6, yjust = 0, xjust = 0)


### Jarrold & Citroën 2013

j13 <- read.table(paste0(pth, "jarrold13.txt"),header=TRUE, sep=",")
names(j13)[1] <- "Year"  #weird insertion of characters in variable name must be corrected

j13 <- j13 %>% rename(dissimilar.pc = DISPC, similar.pc = SIMPC,
                      condition = CONDITION, age = Age, encoding = Encoding,
                      recall = Recall, order = ORDER)

j13 <- j13 %>% rename(digit.span = DSPC, absolute.pse = PSEABSPC, 
                      proportional.pse = PROPPSE)
j13 <- j13 %>% rename(school.year = Year)
j13$school.year[j13$school.year == 1] <- "K"
j13$school.year[j13$school.year == 2] <- "1"
j13$school.year[j13$school.year == 3] <- "2"
j13$school.year[j13$school.year == 4] <- "3"
j13 <- j13 %>% rename(grade.level = school.year)

subj <- 1:116
jar13 <- cbind(j13,subj)

jar13$encoding[jar13$encoding == 1] <- "visual"
jar13$encoding[jar13$encoding == 2] <- "verbal"
jar13$recall[jar13$recall == 1] <- "visual"
jar13$recall[jar13$recall == 2] <- "verbal"

jarrold13 <- jar13 %>% select(subj, grade.level, age, digit.span, order, condition,
                              encoding, recall, dissimilar.pc, similar.pc,
                              absolute.pse, proportional.pse)
jarrold13$grade.level <- as.factor(jarrold13$grade.level)
jarrold13$grade.level <- ordered(jarrold13$grade.level, levels = c("K","1","2","3"))
save(jarrold13, file = "./pkg/data/jarrold13.rda")

### Plot Figure 16 (lower panel)

pd <- aggregate(absolute.pse ~ grade.level+condition, data = jarrold13, FUN = mean)

plot(c(0,5), c(-2,8), type = "n", xlab = "Grade Level",
     ylab = "Phonological Similarity", 
     main = "Absolute Phonological Similarity Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4), labels = levels(pd$grade.level), 
     cex.axis = 0.7)
lines(x = pd$grade.level[pd$condition == 1], 
      y = pd$absolute.pse[pd$condition == 1], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$grade.level[pd$condition == 2], 
      y = pd$absolute.pse[pd$condition == 2], 
      type = "b", lty = 2, pch = 16, col = "darkgrey")
lines(x = pd$grade.level[pd$condition == 3], 
      y = pd$absolute.pse[pd$condition == 3], 
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$grade.level[pd$condition == 4], 
      y = pd$absolute.pse[pd$condition == 4], 
      type = "b", lty = 4, pch = 18, col = "lightgrey")
legend(5, -2, c("visual-verbal","visual-visual","verbal-verbal","verbal-visual"), 
       lty = 1:4, 
       pch=15:18, col = c("black", "darkgrey", "grey","lightgrey"), 
       horiz = F, cex = 0.6, yjust = 0, xjust = 1)



### Macnamara
rm(list=ls())
############### BM8 Similarity #####################
pth  <- "BenchmarksWM.Data/BM8.1.PhonSimEffect/"

## words simple span
simpsersim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Words", range = "A2:F22")
simpserdis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Words", range = "H2:M22")
simpfreesim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Words", range = "A28:F48")
simpfreedis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Words", range = "H28:M48")
simpsersim$recall <- "serial"
simpfreesim$recall <- "free"
simpsim <- rbind(simpsersim, simpfreesim)
simpserdis$recall <- "serial"
simpfreedis$recall <- "free"
simpdis <- rbind(simpserdis, simpfreedis)
simpsim$list <- "similar"
simpdis$list <- "dissimilar"
simpdis <- simpdis %>% rename(span = DisSpan, subj = Subject)
simpdis <- simpdis %>% select(subj, recall, list, span)
simpsim <- simpsim %>% rename(span = SimSpan, subj = Subject)
simpsim <- simpsim %>% select(subj, recall, list, span)
simplespan <- rbind(simpsim, simpdis)

## sentence complex span

sensersim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Sentences", range = "A2:F22")
senserdis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Sentences", range = "H2:M22")
senfreesim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                          sheet = "Sentences", range = "A28:F48")
senfreedis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                          sheet = "Sentences", range = "H28:M48")
sensersim$recall <- "serial"
senfreesim$recall <- "free"
sensim <- rbind(sensersim, senfreesim)
senserdis$recall <- "serial"
senfreedis$recall <- "free"
sendis <- rbind(senserdis, senfreedis)
sensim$list <- "similar"
sendis$list <- "dissimilar"
sendis <- sendis %>% rename(span = DisSpan, subj = Subject)
sendis <- sendis %>% select(subj, recall, list, span)
sensim <- sensim %>% rename(span = SimSpan, subj = Subject)
sensim <- sensim %>% select(subj, recall, list, span)
sentspan <- rbind(sensim, sendis)

### stories complex span

storsersim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                        sheet = "Stories", range = "A2:F23")
storserdis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                        sheet = "Stories", range = "H2:M23")
storfreesim <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Stories", range = "A29:F50")
storfreedis <- read_excel(paste0(pth, "Macnamara et al. (2011) E1 Data.xlsx"), 
                         sheet = "Stories", range = "H29:M50")
storsersim$recall <- "serial"
storfreesim$recall <- "free"
storsim <- rbind(storsersim, storfreesim)
storserdis$recall <- "serial"
storfreedis$recall <- "free"
stordis <- rbind(storserdis, storfreedis)
storsim$list <- "similar"
stordis$list <- "dissimilar"
stordis <- stordis %>% rename(span = DisSpan, subj = Subject)
stordis <- stordis %>% select(subj, recall, list, span)
storsim <- storsim %>% rename(span = SimSpan, subj = Subject)
storsim <- storsim %>% select(subj, recall, list, span)
storyspan <- rbind(storsim, stordis)


simplespan$task <- "word span"
sentspan$task <- "sentence span"
storyspan$task <- "story span"

mac11a <- rbind(simplespan, sentspan, storyspan)

## short sentences span
simpsersim <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                         sheet = "A", range = "A2:F22")
simpserdis <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                         sheet = "A", range = "H2:M22")
simpfreesim <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                          sheet = "A", range = "A28:F48")
simpfreedis <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                          sheet = "A", range = "H28:M48")
simpsersim$recall <- "serial"
simpfreesim$recall <- "free"
simpsim <- rbind(simpsersim, simpfreesim)
simpserdis$recall <- "serial"
simpfreedis$recall <- "free"
simpdis <- rbind(simpserdis, simpfreedis)
simpsim$list <- "similar"
simpdis$list <- "dissimilar"
simpdis <- simpdis %>% rename(span = DisSpan, subj = Subject)
simpdis <- simpdis %>% select(subj, recall, list, span)
simpsim <- simpsim %>% rename(span = SimSpan, subj = Subject)
simpsim <- simpsim %>% select(subj, recall, list, span)
short_sentences <- rbind(simpsim, simpdis)

## long sentences span
simpsersim <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                         sheet = "B", range = "A2:F22")
simpserdis <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                         sheet = "B", range = "H2:M22")
simpfreesim <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                          sheet = "B", range = "A28:F48")
simpfreedis <- read_excel(paste0(pth, "Macnamara et al. (2011) E2 Data.xlsx"), 
                          sheet = "B", range = "H28:M48")
simpsersim$recall <- "serial"
simpfreesim$recall <- "free"
simpsim <- rbind(simpsersim, simpfreesim)
simpserdis$recall <- "serial"
simpfreedis$recall <- "free"
simpdis <- rbind(simpserdis, simpfreedis)
simpsim$list <- "similar"
simpdis$list <- "dissimilar"
simpdis <- simpdis %>% rename(span = DisSpan, subj = Subject)
simpdis <- simpdis %>% select(subj, recall, list, span)
simpsim <- simpsim %>% rename(span = SimSpan, subj = Subject)
simpsim <- simpsim %>% select(subj, recall, list, span)
long_sentences <- rbind(simpsim, simpdis)

short_sentences$length <- "short"
long_sentences$length <- "long"

mac11b <- rbind(short_sentences, long_sentences)

mac11a$exp <- 1
mac11b$exp <- 2
mac11a$length <- "short"
mac11b$task <- "sentence span"

macnamara11 <- rbind(mac11a, mac11b)

macnamara11 <- macnamara11 %>% select(exp, subj, task, length, recall, list, span)

save(macnamara11, file = "./pkg/data/macnamara11.rda")

### Plot Exp 1 Barplot:
psd <- macnamara11[which(macnamara11$exp == 1 & macnamara11$recall == "serial"),]
pd <- aggregate(span ~ task+list, data = psd, FUN = mean)
pd <- pd[c(6,3,4,1,5,2),] 
barspacing <- c(0.2,0.2,0.7,0.2,0.7,0.2)
legendtext <- c("","Words", "Sentences", "Stories","")
bp = barplot(pd$span, space = barspacing, col=c("dark gray","light gray","dark gray",
                                               "light gray","dark gray","light gray"),
             ylab="Serial Span", xlab="Task", axisnames=T, 
             ylim = c(0,1), xpd = T)
axis(1, at = c(0,1.3,4.2,7.1,8.2), labels=legendtext, cex.axis=1, outer = F)
legend(8, 1, c("similar", "dissimilar"), 
       pch=c(15,15), col = c("darkgrey", "lightgrey"), 
       horiz = F, cex = 0.8, yjust = 1, xjust = 1)
###################################################

rm(list=ls())
############### BM8 Similarity #####################
pth  <- "BenchmarksWM.Data/BM8.2.ItemProbeSim/"



CD <- read.table(paste0(pth, "WeijiChangeDetection.dat"), header=F)
names(CD) <- c("id", "setsize", "delta", "response")

# plot CD
nbins <- 11
deltabins <- 0.0001+seq(from=0, to=0.5*pi, length.out=nbins)
idvector <- unique(CD$id)
ssvector <- unique(CD$setsize)

cd <- CD
cd$bin[cd$delta == 0] <- 0.0
for (b in 2:nbins) {
  cd$bin[cd$delta>deltabins[b-1] & cd$delta<deltabins[b]] <- deltabins[b] 
}

colors <- c("black", "gray50", "gray80", "white")

source("BenchmarksWM.Data/Functions/lineplot.ci.R")

lineplot.ci(deltabins, propResp, xdim=2, xlab="Size of Change (rad)", ylab="P('change')", 
            pt=21:24, ptcol=colors, cex=1.2)
legend(max(deltabins),0, legend=c("N=1", "N=2", "N=4", "N=8"), pch=c(21:24), pt.cex=1.2,
       pt.bg=colors, lty=1, xjust=1,yjust=0)

keshvari13 <- cd %>% select(id,setsize,delta,bin,response)
keshvari13 <- keshvari13 %>% rename(subj = id, deltabin = bin)
save(keshvari13, file = "./pkg/data/keshvari13.rda")

### Plot Figure 2C in Keshvari et al. (2013)

pd <- aggregate(response ~ deltabin+setsize, data = keshvari13, FUN = mean)

plot(c(0,1.7), c(0,1.0), type = "n", xlab = "Size of Change (rad)",
     ylab = "Proportion \"change\"", 
     main = "Change Detection in Visual WM", xaxt = "n")
axis(side = 1, at = c(0.0,0.5,1.0,1.5), labels = c(0.0,0.5,1.0,1.5), 
     cex.axis = 1.0)
lines(x = pd$deltabin[pd$setsize == 2], 
      y = pd$response[pd$setsize == 2], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$deltabin[pd$setsize == 4], 
      y = pd$response[pd$setsize == 4], 
      type = "b", lty = 2, pch = 16, col = "darkgrey")
lines(x = pd$deltabin[pd$setsize == 6], 
      y = pd$response[pd$setsize == 6], 
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$deltabin[pd$setsize == 8], 
      y = pd$response[pd$setsize == 8], 
      type = "b", lty = 4, pch = 18, col = "lightgrey")
legend(1.6, 0, c("N=1","N=2","N=4","N=8"), 
       lty = 1:4, 
       pch=15:18, col = c("black", "darkgrey", "grey","lightgrey"), 
       horiz = F, cex = 0.6, yjust = 0, xjust = 1)





