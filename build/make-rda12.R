## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM12 Individual Differences #####################
pth  <- "BenchmarksWM.Data/BM12.1.PositiveManifold/"

# Read data
kane04 <- read_excel(paste0(pth,"Kane et al 2004, FINAL DATA for Benchmarks.xlsx"),
                       col_names=T, range="A4:AYS239")

# Explanation of column names:

# The initial columns are AGGREGATE SCORES FOR STM TASKS (Columns 2-7), WM TASKS (8-13), & REASONING TAKSKS (14-26)
# wor=word span; let=letter span; dig = digit span; bal = ball span; arr = arrow span; mat = matrix span; ope = operation span; cou = counting span; rea = reading span; nav = navigation span; sym = symmetry span; rot = rotation span; etsin = ets inferences; papfo = paper folding; afqan = afq analogies; datsr = dat space relations; remass = remote associates; afqrt = afq rotated blocks; etssy = ets nonsense syllogisms; etssd = ets surface development; afqrc = afq reading comprehension; etsfb = ets form board; wasim = wasi matrices; raven = ravens matrices; beta3 = beta 3 matrices

aggdata <- kane04[,1:26]
names(aggdata) <- c("subj", "word.span", "letter.span", "digit.span", "ball.span",
                    "arrow.span", "matrix.span", "operation.span", "counting.span",
                    "reading.span", "navigation.span", "symmetry.span", "rotation.span",
                    "ets.inference", "paper.folding", "afq.analogies",
                    "dat.space.relations", "remote.associates", "afq.rotated.blocks",
                    "ets.nonsense.syllogisms", "ets.surface.development",
                    "afq.reading.comprehension", "ets.form.board", "wasi.matrices",
                    "raven.matrices", "beta3.matrices")
# save(kane04, file = "./pkg/data/kane04.rda")

agg.long <- gather(aggdata, key = "task", value = "acc", 2:26)

agg.long$type <- ""
agg.long$type[agg.long$task %in% c("word.span", "letter.span", "digit.span", "ball.span", "arrow.span", "matrix.span")] <- "STM"
agg.long$type[agg.long$task %in% c("operation.span", "counting.span",
                                   "reading.span", "navigation.span", "symmetry.span","rotation.span")] <- "WM"
agg.long$type[agg.long$task %in% c("ets.inference", "paper.folding", "afq.analogies","dat.space.relations", "remote.associates","afq.rotated.blocks", "ets.nonsense.syllogisms", "ets.surface.development", "afq.reading.comprehension","ets.form.board", "wasi.matrices", "raven.matrices",
                                   "beta3.matrices")] <- "reasoning"

kane04s <- agg.long %>% select(subj, type, task, acc)

#save(kane04s, file = "./pkg/data/kane04s.rda")

# The next columns are STM Task item level recall accuray (items a-h[max]), by trial number (01-21[max]) for the 6 STM tasks; the variable name codes the task, as listed above
# This is followed by WM Task item level recall accuracyy (items a-f[max]), by trial number (01-15[max]) for the 6 WM tasks; the variable name codes the task

# Next follows the WM Task trial level recall accy (UPC score; proportion correct), tasks coded as above;
# after that the STM Task trial level recall accy (UPC score; proportion correct), tasks coded as above

# The next columns are the WM task processing-component accuracy by item, by trial; variable names start with "p_" followed by the task code; with trial number (01-15) and items (a-f)

# After that, WM task processing-component accuracy by trial (proportion correct); variable names start with "p_" followed by the task code; with trial number (01-15) but no letter following

# Finally, the last 6 columns are the WM Task processing-component  Mean accuracy by task: variable names start with "p_" followed by the task code, no number or letter following

## these are the raw data. I don't include them in the dataset.
## the dataset is just trial-level data as stimuli aren't available anyway
## raw data STM tasks
# darrow <- kane04[,c(1,27:86)]
# dword <- kane04[,c(1,87:167)]
# dmatspan <- kane04[,c(1,168:248)]
# dletspan <- kane04[,c(1,249:353)]
# dballspan <- kane04[,c(1,354:413)]
# ddigspan <- kane04[,c(1,414:539)]
#
# ## raw data WM tasks
#
# dopspan <- kane04[,c(1,540:581)]
# dnavspan <- kane04[,c(1,582:623)]
# dcountspan <- kane04[,c(1,624:683)]
# dsymspan <- kane04[,c(1,684:725)]
# dreadingspan <- kane04[,c(1,726:767)]
# drotspan <- kane04[,c(1,768:809)]


## that was unnecessary
## trial-level data WM


### trial-level data STM

## note: arr: 15; wor: 18; mat: 18; let: 21; bal: 15; dig: 21

tarr <- kane04[c(1,885:899)]
twor <- kane04[c(1,900:917)]
tmat <- kane04[c(1,918:935)]
tlet <- kane04[c(1,936:956)]
tbal <- kane04[c(1,957:971)]
tdig <- kane04[c(1,972:992)]

## change for each
## create a function

add_cells <- function(data) {
  dd <- dim(data)[2]
  data[,dd:22] <- NA
  c_vec <- c()
  for (mc in dd:22) {
    i <- (mc-dd+1)
    c_vec[i] <- paste0("abc", as.character(mc))
  }
  names(data)[dd:22] <- c_vec
  return(data)
}

## do for each sub dataset

tarr <- add_cells(tarr)
twor <- add_cells(twor)
tmat <- add_cells(tmat)
tlet <- add_cells(tlet)
tbal <- add_cells(tbal)
tdig <- add_cells(tdig)

# dim(tarr)[2]
# dd <- dim(tbal)[2]
# tbal[,dd:22] <- NA
# names(tbal)[dd:22] <- c("asd16","ope17","ope18","ert19","ewr20","wer21","har22")

## done
## do for WM

topspan <- kane04[c(1,810:821)]
tnavspan <- kane04[c(1,822:833)]
tcountspan <- kane04[c(1,834:848)]
tsymspan <- kane04[c(1,849:860)]
treadingspan <- kane04[c(1,861:872)]
trotspan <- kane04[c(1,873:884)]

topspan <- add_cells(topspan)
tnavspan <- add_cells(tnavspan)
tcountspan <- add_cells(tcountspan)
tsymspan <- add_cells(tsymspan)
treadingspan <- add_cells(treadingspan)
trotspan <- add_cells(trotspan)

# dim(topspan)[2]
# dd <- dim(topspan)[2]
# topspan[,dim(topspan)[2]:22] <- NA
# names(topspan)[dd:22] <- c("asd16","ope17","ope18","ert19","ewr20","wer21","har22")

### done

### create long format for each dataset and cbind it to wide data set for cor matrix

make_long <- function(data, name) {
  d <- gather(data, key = "trial", value = "acc", 2:dim(data)[2])
  d$trial <- as.numeric(substr(d$trial,4,5))
  names(d)[names(d) == "acc"] <- name
  return(d)
}

kane06cor <- make_long(tarr,"arrow.span")
wordsp <- make_long(twor, "word.span")
matsp <- make_long(tmat, "matrix.span")
letsp <- make_long(tlet, "letter.span")
balsp <- make_long(tbal, "ball.span")
digsp <- make_long(tdig, "digit.span")

opsp <- make_long(topspan, "operation.span")
navsp <- make_long(tnavspan, "navigation.span")
countsp <- make_long(tcountspan, "counting.span")
symsp <- make_long(tsymspan, "symmetry.span")
redsp <- make_long(treadingspan, "reading.span")
rotsp <- make_long(trotspan, "rotation.span")

dfinal <- cbind(kane06cor, wordsp$word.span, matsp$matrix.span, letsp$letter.span, balsp$ball.span, digsp$digit.span, opsp$operation.span, navsp$navigation.span, countsp$counting.span, symsp$symmetry.span, redsp$reading.span, rotsp$rotation.span)

kane04 <- dfinal %>% rename(subj = s_num, word.span = `wordsp$word.span`,
                            matrix.span = `matsp$matrix.span`,
                            letter.span = `letsp$letter.span`,
                            ball.span = `balsp$ball.span`, digit.span = `digsp$digit.span`,
                            operation.span = `opsp$operation.span`,
                            navigation.span = `navsp$navigation.span`,
                            counting.span = `countsp$counting.span`,
                            symmetry.span = `symsp$symmetry.span`,
                            reading.span = `redsp$reading.span`,
                            rotation.span = `rotsp$rotation.span`)
## 14:26 in aggdata
id_vec <- sort(unique(kane04$subj))
trial_vec <- sort(unique(kane04$trial))
kane04[,15:27] <- 0
for (ss in id_vec) {
  for (tr in trial_vec) {
    kane04[kane04$subj == ss & kane04$trial == tr,15:27] <- aggdata[aggdata$subj == ss,14:26]
  }
}

names(kane04)[15:27] <- names(aggdata)[14:26]

check <- aggregate(cbind(arrow.span,word.span,matrix.span,letter.span,
                         ball.span) ~ subj, data = kane04, FUN = mean)


save(kane04, file = "./pkg/data/kane04.rda")


####################################################

## unsworth 2010

pth  <- "BenchmarksWM.Data/BM12.4.PrimarySecondaryMem/"

uns10 <- read_excel(paste0(pth, "UnsworthSpillersBrewer.2010.xlsx"), col_names = T,
                    range = "A1:W141")

uns10 <- uns10 %>% rename(subj =ID, operation.span = Ospan, symmetry.span = Symspan,
                          reading.span = Rspan, number.series = ns, verbal.analogies = ang,
                          verbal.SAT = VSAT, quantitative.SAT = QSAT, free.recall = IFR,
                          prim.mem.1 = PM1, prim.mem.2 = PM2, second.mem.1 = SM1,
                          second.mem.2 = SM2)

unsworth10 <- uns10

save(unsworth10, file = "./pkg/data/unsworth10.rda")

## plot???


### Cowan et al. (1998) Exp 2

pth  <- "BenchmarksWM.Data/BM12.5.CorrelArticSpeed/"

# Read data
Cowan98 <- read_excel(paste0(pth, "Cowan.etal.JEPG.1998.expt2.xls"), sheet=2, col_names=T)

# Explanation of column names:

# SubNo	subject number
# age	in years only
# sex	male or female
# ethnic	as shown, self-identified ethnicity:  caucasian, African-American (AfAmer), Asian-American (AsAmer), or unknown (UK)
#
# DigSp	digit span
# LetSp	letter span
#
# Let1	searching for 1 letter, number of letters found in time period allowed, 30 s
# Dig1	searching for 1 digit
# Let3	searching for 3 letters
# Dig3	searching for 3 digits
# Let5	searching for 5 letters
# Dig5	searching for 5 digits
# LETINTER	intercept of letter search function across set sizes 1, 3, 5
# LETSLOPE	slope of letter search function (ms per memory set item)
# DIGINTER	intercept of digit search function across set sizes 1, 3, 5
# DIGSLOPE	slope of digit search function (ms per memory set item)
#
# Alpha	speed of covert recitation of alphabet, silent repetitions of A-Z in 60 s
# Count	speed of covert counting, silent repetitions of 1-10 in 30 s

cowan98 <- Cowan98 %>% rename(subj = SubNo, ethnicity = ethnic, digit.span = DigSp,
                              letter.span = LetSp, letter.search.1 = Let1,
                              digit.search.1 = Dig1, letter.search.3 = Let3,
                              digit.search.3 = Dig3, letter.search.5 = Let5,
                              digit.search.5 = Dig5, letter.search.intercept = LETINTER,
                              letter.search.slope = LETSLOPE, digit.search.intercept = DIGINTER,
                              digit.search.slope = DIGSLOPE, rapid.alphabet = Alpha,
                              rapid.counting = Count)

save(cowan98, file="./pkg/data/cowan98.rda")









