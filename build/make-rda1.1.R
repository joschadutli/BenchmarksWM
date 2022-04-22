## Convert data sets to R package format (.rda)
## Author: Andy J. Wills
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
############### BM1.1 #####################
pth  <- "BenchmarksWM.Data/BM1.1.SetsizeAccuracy/"

## Issues w/ original R script:
## - Duplicate data sets (.xls and .txt).
## - BM1.1.SetsizeAccuracy.R:
##     - requires a directory "functions" which is absent
##     - assumes case-insentive filenames

### Unsworth & Engle (2006a) ###

## Summary of changes:
## - Converted to long format.
## - Test and length separated as variables.
## - Simple/complex variable added to easily reproduce results as reported.
## - Variable levels given meaningful names.
## - Non-benchmark tests excluded.
fnam  <- paste0(pth, "Unsworth.Engle.Listlength.txt")
rawd <- read_tsv(fnam)
un <- gather(rawd, worupc2:wmset5, key="comb", value="acc")
un$size <- as.numeric(substr(un$comb, nchar(un$comb), nchar(un$comb)))
un$test <- substr(un$comb, 1, nchar(un$comb)-1)
un <- un %>% select(subject, test, size, acc)
un$test <- recode(un$test, worupc = "word", letupc = "letter",
                  opupc = "operation", rspupc = "reading")
un <- un %>% filter(test != "wmset") %>% filter(test != "stmset")
un <- un %>% arrange(subject, test, size)
un$type <- recode(un$test, word = "simple", letter = "simple",
                  operation = "complex", reading = "complex")
un <- un %>% select(subject, type, test, size, acc)
unsworth06a <- un
save(unsworth06a, file = "pkg/data/unsworth06a.rda", compress = "xz")
rm(un)

### Bunting et al. (2006) ###

## Summary of changes:
## - Converted to long format
## - Speed, span, and position, separated as variables
## - Variables given meaningful names
## - Handedness removed as all participants were right handed.

fnam  <- paste0(pth, "Bunting.Cowan.Running.xls")
rawd <- read_excel(fnam, sheet=3)
bu <- gather(rawd, f7sp7_ac:s1sp1_ac, key="comb", value="acc")
bu$speed <- substr(bu$comb, 1, 1)
bu$span <- as.integer(substr(bu$comb, 2, 2))
bu$pos <- as.integer(substr(bu$comb, 5, 5))
bu <- bu %>% select(Subject, Gender, speed, span, pos, acc)
colnames(bu) <- c("subject", "gender", "speed", "span", "pos", "acc")
bu$speed <- recode(bu$speed, f = "fast", s = "slow")
bu$subject <- as.integer(bu$subject)
bunting06 <- bu
save(bunting06, file = "./pkg/data/bunting06.rda", compress = "xz")
rm(bu)

### McElree & Dosher (1989) ###
fnam  <- paste0(pth, "/mcelree89.csv")
mcelree89 <- read_csv(fnam)
save(mcelree89, file = "./pkg/data/mcelree89.rda", compress = "xz")

### Jonides et al. (1997) ###
NbackPE <- c(0.03, 0.05, 0.065, 0.115)
acc  <- 1-NbackPE
back  <- 0:3
jonides97  <- data.frame(back, acc)
save(jonides97, file = "./pkg/data/jonides97.rda", compress = "xz")

### Verhaeghen & Basak (2005)
NbackVerhaeghenY <- c(0.97, 0.96, 0.945, 0.92, 0.86)
acc  <- NbackVerhaeghenY
back  <- 1:5
verhaeghen05  <- data.frame(back, acc)
save(verhaeghen05, file = "./pkg/data/verhaeghen05.rda",
     compress = "xz")

### Oberauer & Kliegel (2001)

## Summary of changes relative to Oberauer 2019:

## Converted to long format
## Measure and item seperated as variables
## Column added to indicate age group

fnam  <- paste0(pth, "/Oberauer.Kliegl.MU1.DAT")

## Load Part 1
colnames1 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit",
               "corrval1", "resp1", "correct1", "rt1", "corrval2", "resp2",
               "correct2", "rt2", "corrval3", "resp3", "correct3", "rt3",
               "corrval4", "resp4", "correct4", "rt4")
mutaf1 <- read.table(fnam, header=F, fill=T, col.names=colnames1)

## Load Part 2
colnames2 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit",
               "corrval1", "resp1", "correct1", "rt1", "corrval2", "resp2",
               "correct2", "rt2", "corrval3", "resp3", "correct3", "rt3",
               "corrval4", "resp4", "correct4", "rt4", "corrval5", "resp5",
               "correct5", "rt5", "corrval6", "resp6", "correct6", "rt6")
fnam  <- paste0(pth, "/Oberauer.Kliegl.MU2.DAT")
mutaf2 <- read.table(fnam, header=F, fill=T, col.names=colnames2)

## Label parts
mutaf1$exp = 1
mutaf2$exp = 2

## Remove setsize zero trials - not sure what these are...
mutaf1 <- mutaf1[mutaf1$setsize>0,]

## Filter to trials with maximum presentation duration (6s)
## and to young adults (id < 30)

#mutaf2 <- mutaf2 %>% filter(pt0 > 5999) %>% filter(id < 30)

## Select needed columns
mutaf1 <- mutaf1 %>%
    select(id, exp, setsize, trial, pt0, corrval1:rt4)
mutaf2 <- mutaf2 %>%
    select(id, exp, setsize, trial, pt0, corrval1:rt6)

## Convert to long format and arrange order
mutaf1l <- mutaf1 %>%
    gather(key = "key", value = "val", corrval1:rt4) %>%
    arrange(id, setsize, trial, key)

mutaf2l <- mutaf2 %>%
    gather(key = "key", value = "val", corrval1:rt6) %>%
    arrange(id, setsize, trial, key)

## Combine studies
mutafX  <- rbind(mutaf1l, mutaf2l)

## Remove NA
mutafX  <- mutafX[!is.na(mutafX$val),]

## Split 'key' into measure and item
mutafX$item <- substr(mutafX$key,
                       nchar(mutafX$key),
                       nchar(mutafX$key))

mutafX$measure <- substr(mutafX$key, 1,
                         nchar(mutafX$key) - 1)

## Add column for participant age
mutafX$age <- "older"
mutafX$age[mutafX$id < 30] <- "young"

## Remove key and rearrange order of columns
mutafX <- mutafX %>%
    select(age, id, exp, setsize, pt0, trial, item, measure, val)

## Standardize column names
colnames(mutafX)  <- c("age", "subject", "part", "size", "ptime", "trial",
                       "item", "measure", "val")

## Create final dataframe, with human-readble ordering of rows
oberauer01 <- mutafX %>% arrange(subject, part, size, trial, item, measure)
save(oberauer01, file = "./pkg/data/oberauer01.rda", compress = "xz")

### Change detection: Adam et al (2015)
fnam  <- paste0(pth, "Adam.ChangeDet.dat")
adam15 <- read.table(fnam, header=F)
colnames(adam15) <- c("subject", "size", "change", "acc")
save(adam15, file = "./pkg/data/adam15.rda", compress = "xz")


