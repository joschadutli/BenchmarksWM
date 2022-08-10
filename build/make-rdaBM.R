## Create data sets for BM files and convert to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
############### BM1.1 #####################

## read in data:

library(readxl)
library(dplyr)

pth  <- "BenchmarksWM.Data/BMOverview/"


## Benchmarks 1.1

fnam <- paste0("../", pth, "benchmarks1.1.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmark1.1 <- dat

save(benchmark1.1, file = "./data/benchmark1.1.rda", compress = "gzip")

# promptData(benchmark1.1, filename = "./man/benchmark1.1.Rd")

## Benchmarks 1.2

fnam <- paste0("../", pth, "benchmarks1.2.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmark1.2 <- dat

save(benchmark1.2, file = "./data/benchmark1.2.rda", compress = "gzip")

# promptData(benchmark1.2, filename = "./man/benchmark1.2.Rd")

## Benchmarks 1.3

fnam <- paste0("../", pth, "benchmarks1.3.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmark1.3 <- dat

save(benchmark1.3, file = "./data/benchmark1.3.rda", compress = "gzip")

# promptData(benchmark1.3, filename = "./man/benchmark1.3.Rd")

## Benchmarks 2.1

fnam <- paste0("../", pth, "benchmarks2.1.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmark2.1 <- dat

save(benchmark2.1, file = "./data/benchmark2.1.rda", compress = "gzip")

# promptData(benchmark2.1, filename = "./man/benchmark2.1.Rd")

## Benchmarks 2.4

fnam <- paste0("../", pth, "benchmarks2.4.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmark2.4 <- dat

save(benchmark2.4, file = "./data/benchmark2.4.rda", compress = "gzip")

# promptData(benchmark2.4, filename = "./man/benchmark2.4.Rd")

## Benchmarks 3

fnam <- paste0("../", pth, "benchmarks3.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark3 <- dat

save(benchmark3, file = "./data/benchmark3.rda", compress = "gzip")

# promptData(benchmark3, filename = "./man/benchmark3.Rd")

## Benchmarks 4


fnam <- paste0("../", pth, "benchmarks4.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark4 <- dat

save(benchmark4, file = "./data/benchmark4.rda", compress = "gzip")

# promptData(benchmark4, filename = "./man/benchmark4.Rd")


## Benchmarks 5

fnam <- paste0("../", pth, "benchmarks5.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark5 <- dat

save(benchmark5, file = "./data/benchmark5.rda", compress = "gzip")

# promptData(benchmark5, filename = "./man/benchmark5.Rd")


## Benchmarks 6

fnam <- paste0("../", pth, "benchmarks6.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark6 <- dat

save(benchmark6, file = "./data/benchmark6.rda", compress = "gzip")

# promptData(benchmark6, filename = "./man/benchmark6.Rd")


## Benchmarks 7

fnam <- paste0("../", pth, "benchmarks7.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark7 <- dat

save(benchmark7, file = "./data/benchmark7.rda", compress = "gzip")

# promptData(benchmark7, filename = "./man/benchmark7.Rd")


## Benchmarks 8

fnam <- paste0("../", pth, "benchmarks8.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark8 <- dat

save(benchmark8, file = "./data/benchmark8.rda", compress = "gzip")

# promptData(benchmark8, filename = "./man/benchmark8.Rd")


## Benchmarks 9

fnam <- paste0("../", pth, "benchmarks9.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark9 <- dat

save(benchmark9, file = "./data/benchmark9.rda", compress = "gzip")

# promptData(benchmark9, filename = "./man/benchmark9.Rd")


## Benchmarks 10

fnam <- paste0("../", pth, "benchmarks10.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark10 <- dat

save(benchmark10, file = "./data/benchmark10.rda", compress = "gzip")

# promptData(benchmark10, filename = "./man/benchmark10.Rd")


## Benchmarks 11

fnam <- paste0("../", pth, "benchmarks11.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark11 <- dat

save(benchmark11, file = "./data/benchmark11.rda", compress = "gzip")

# promptData(benchmark11, filename = "./man/benchmark11.Rd")


## Benchmarks 12

fnam <- paste0("../", pth, "benchmarks12.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmark12 <- dat

save(benchmark12, file = "./data/benchmark12.rda", compress = "gzip")

# promptData(benchmark12, filename = "./man/benchmark12.Rd")



## Benchmarks Overview

fnam <- paste0("../", pth, "benchmarks_overview.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarks.overview <- dat

save(benchmarks.overview, file = "./data/benchmarks.overview.rda", compress = "gzip")

# promptData(benchmarks.overview, filename = "./man/benchmarks.overview.Rd")











# x <- c("dataset", "manipulation", "response.variable", "task",
#        "memory.test", "stimulus.material")
# uns <- c("unsworth06a", "set size", "recall accuracy",
#          "simple vs. complex span",
#          "serial recall", "verbal")
# bun <- c("bunting06", "set size", "recall accuracy", "running span",
#          "serial recall", "verbal (auditorily presented)")
# mce <- c("mcelree89", "set size", "recognition accuracy", "simple span",
#          "item recognition", "verbal")
# jon <- c("jonides97", "increase of N", "recall accuracy", "N-back",
#          "single-item recall", "verbal")
# ver <- c("verhaeghen05", "increase of N", "recall accuracy", "modified N-back",
#          "single-item recall", "verbal")
# obe <- c("oberauer01", "set size", "recall accuracy", "WM updating",
#          "random order recall", "digits")
# ada <- c("adam15", "set size", "accuracy", "change detection",
#          "change detection", "visual (colored squares)")
#
# d1.1 <- as.data.frame(rbind(uns, bun, mce, jon, ver, obe, ada))
#
# names(d1.1) <- x
#
# row.names(d1.1) <- 1:7
#
# benchmark1.1 <- d1.1
#
# fnam <- paste0("../", pth, "benchmarks1.1.xlsx")
#
# write.xlsx(benchmark1.1, file=fnam)
#
# # save(benchmark1.1, file = "./data/benchmark1.1.rda", compress = "gzip")
#
# # promptData(benchmark1.1, name = "benchmark1.1")
#
# ### benchmark1.2
#
# x <- c("dataset", "manipulation", "response.variable", "task",
#        "memory.test", "stimulus.material")
# don <- c("donkin12b", "set size and presentation rate", "response times",
#          "Sternberg task",
#          "recognition", "verbal (digits)")
# gil <- c("gilchrist14", "set size", "response times", "change detection",
#          "recognition", "visual")
# she <- c("shepherdson18", "set size and retro cues", "response times",
#          "retro cue paradigm",
#          "recognition", "verbal/visual")
# tow <- c("towse08", "list length", "response times", "complex span",
#          "serial recall", "verbal")
#
# d1.2 <- as.data.frame(rbind(don, gil, she, tow))
#
# names(d1.2) <- x
#
# row.names(d1.2) <- 1:length(d1.2$dataset)
#
# benchmark1.2 <- d1.2
#
# fnam <- paste0("../", pth, "benchmarks1.2.xlsx")
#
# write.xlsx(benchmark1.2, file=fnam)
#
# # save(benchmark1.2, file = "./data/benchmark1.2.rda", compress = "gzip")
#
# ## benchmark1.3
#
# x <- c("dataset", "manipulation", "response.variable", "task",
#        "memory.test", "stimulus.material")
#
# che <- c("chen09", "number of chunks", "WM capacity", "lists of words",
#          "free recall", "verbal (words, word-pairs)")
#
# d1.3 <- as.data.frame(matrix(data = che, nrow = 1, ncol = 6))
#
# names(d1.3) <- x
#
# row.names(d1.3) <- 1
#
# benchmark1.3 <- d1.3
#
# fnam <- paste0("../", pth, "benchmarks1.3.xlsx")
#
# write.xlsx(benchmark1.3, file=fnam)
#
# # save(benchmark1.3, file = "./data/benchmark1.3.rda", compress = "gzip")
#
# #### benchmark2.1
# x <- c("dataset", "manipulation", "response.variable", "task",
#        "memory.test", "stimulus.material")
# flo <- c("floden10", "filled retention interval", "proportion correct",
#          "Brown-Peterson", "recall", "verbal (words)")
# lew <- c("lewandowsky10", "inter-stimulus interval", "proportion correct",
#          "complex span", "recall", "verbal")
# ric <- c("ricker14", "unfilled retention interval", "accuracy",
#          "change detection", "change detection", "visual")
#
# d2.1 <- as.data.frame(rbind(flo, lew, ric))
#
# names(d2.1) <- x
#
# row.names(d2.1) <- 1:dim(d2.1)[1]
#
# benchmark2.1 <- d2.1
#
# fnam <- paste0("../", pth, "benchmarks2.1.xlsx")
#
# write.xlsx(benchmark2.1, file=fnam)
#
# # save(benchmark2.1, file = "./data/benchmark2.1.rda", compress = "gzip")
#
# ##### benchmark2.4
#
# x <- c("dataset", "manipulation", "response.variable", "task",
#        "memory.test", "stimulus.material")
# tan <- c("tan08", "presentation duration", "serial position accuracy",
#          "overt rehearsal", "serial recall", "verbal (words)")
# bay <- c("bays11", "presentation duration and set size",
#          "error degrees", "continuous reproduction",
#          "serial recall", "visual (orientations)")
# gre <- c("grenfell13", "presentation duration", "serial position accuracy",
#          "simple span", "free recall", "verbal (words)")
# ric <- c("ricker17", "presentation duration", "response error",
#          "continuous reproduction", "serial recall", "visual (orientations)")
#
# d2.4 <- as.data.frame(rbind(flo, lew, ric))
#
# names(d2.4) <- x
#
# row.names(d2.4) <- 1:dim(d2.4)[1]
#
# benchmark2.4 <- d2.4
#
# fnam <- paste0("../", pth, "benchmarks2.4.xlsx")
#
# write.xlsx(benchmark2.4, file=fnam)

# save(benchmark2.4, file = "./data/benchmark2.4.rda", compress = "gzip")

##### benchmark3





