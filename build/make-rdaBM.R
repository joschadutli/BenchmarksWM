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

benchmarkWM1.1 <- dat

save(benchmarkWM1.1, file = "./data/benchmarkWM1.1.rda", compress = "gzip")

# promptData(benchmarkWM1.1, filename = "./man/benchmarkWM1.1.Rd")

## Benchmarks 1.2

fnam <- paste0("../", pth, "benchmarks1.2.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmarkWM1.2 <- dat

save(benchmarkWM1.2, file = "./data/benchmarkWM1.2.rda", compress = "gzip")

# promptData(benchmarkWM1.2, filename = "./man/benchmarkWM1.2.Rd")

## Benchmarks 1.3

fnam <- paste0("../", pth, "benchmarks1.3.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmarkWM1.3 <- dat

save(benchmarkWM1.3, file = "./data/benchmarkWM1.3.rda", compress = "gzip")

# promptData(benchmarkWM1.3, filename = "./man/benchmarkWM1.3.Rd")

## Benchmarks 2.1

fnam <- paste0("../", pth, "benchmarks2.1.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmarkWM2.1 <- dat

save(benchmarkWM2.1, file = "./data/benchmarkWM2.1.rda", compress = "gzip")

# promptData(benchmarkWM2.1, filename = "./man/benchmarkWM2.1.Rd")

## Benchmarks 2.4

fnam <- paste0("../", pth, "benchmarks2.4.xlsx")
dat <- read_excel(path=fnam, sheet = "Sheet 1")

benchmarkWM2.4 <- dat

save(benchmarkWM2.4, file = "./data/benchmarkWM2.4.rda", compress = "gzip")

# promptData(benchmarkWM2.4, filename = "./man/benchmarkWM2.4.Rd")

## Benchmarks 3

fnam <- paste0("../", pth, "benchmarks3.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM3 <- dat

save(benchmarkWM3, file = "./data/benchmarkWM3.rda", compress = "gzip")

# promptData(benchmarkWM3, filename = "./man/benchmarkWM3.Rd")

## Benchmarks 4


fnam <- paste0("../", pth, "benchmarks4.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM4 <- dat

save(benchmarkWM4, file = "./data/benchmarkWM4.rda", compress = "gzip")

# promptData(benchmarkWM4, filename = "./man/benchmarkWM4.Rd")


## Benchmarks 5

fnam <- paste0("../", pth, "benchmarks5.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM5 <- dat

save(benchmarkWM5, file = "./data/benchmarkWM5.rda", compress = "gzip")

# promptData(benchmarkWM5, filename = "./man/benchmarkWM5.Rd")


## Benchmarks 6

fnam <- paste0("../", pth, "benchmarks6.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM6 <- dat

save(benchmarkWM6, file = "./data/benchmarkWM6.rda", compress = "gzip")

# promptData(benchmarkWM6, filename = "./man/benchmarkWM6.Rd")


## Benchmarks 7

fnam <- paste0("../", pth, "benchmarks7.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM7 <- dat

save(benchmarkWM7, file = "./data/benchmarkWM7.rda", compress = "gzip")

# promptData(benchmarkWM7, filename = "./man/benchmarkWM7.Rd")


## Benchmarks 8

fnam <- paste0("../", pth, "benchmarks8.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM8 <- dat

save(benchmarkWM8, file = "./data/benchmarkWM8.rda", compress = "gzip")

# promptData(benchmarkWM8, filename = "./man/benchmarkWM8.Rd")


## Benchmarks 9

fnam <- paste0("../", pth, "benchmarks9.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM9 <- dat

save(benchmarkWM9, file = "./data/benchmarkWM9.rda", compress = "gzip")

# promptData(benchmarkWM9, filename = "./man/benchmarkWM9.Rd")


## Benchmarks 10

fnam <- paste0("../", pth, "benchmarks10.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM10 <- dat

save(benchmarkWM10, file = "./data/benchmarkWM10.rda", compress = "gzip")

# promptData(benchmarkWM10, filename = "./man/benchmarkWM10.Rd")


## Benchmarks 11

fnam <- paste0("../", pth, "benchmarks11.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM11 <- dat

save(benchmarkWM11, file = "./data/benchmarkWM11.rda", compress = "gzip")

# promptData(benchmarkWM11, filename = "./man/benchmarkWM11.Rd")


## Benchmarks 12

fnam <- paste0("../", pth, "benchmarks12.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarkWM12 <- dat

save(benchmarkWM12, file = "./data/benchmarkWM12.rda", compress = "gzip")

# promptData(benchmarkWM12, filename = "./man/benchmarkWM12.Rd")



## Benchmarks Overview

fnam <- paste0("../", pth, "benchmarks_overview.xlsx")
dat <- read_excel(path=fnam, sheet = "Tabelle1")

benchmarksWM.overview <- dat

save(benchmarksWM.overview, file = "./data/benchmarksWM.overview.rda", compress = "gzip")

# promptData(benchmarksWM.overview, filename = "./man/benchmarksWM.overview.Rd")











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





