## Create data sets for BM files and convert to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
############### BM1.1 #####################

a <- rep(1,7)

datNames <- c("descriptions", "unsworth06a", "bunting06", "mcelree89", "jonides97", "verhaeghen05", "oberauer01", "adam15")

x <- c("manipulation", "response variable", "task",
       "memory test", "stimulus material")
uns <- c("set size", "recall accuracy", "simple vs. complex span",
         "serial recall", "verbal")
bun <- c("set size", "recall accuracy", "running span",
         "serial recall", "verbal (auditorily presented)")
mce <- c("set size", "recognition accuracy", "simple span",
         "item recognition", "verbal")
jon <- c("increase of N", "recall accuracy", "N-back",
         "single-item recall", "verbal")
ver <- c("increase of N", "recall accuracy", "modified N-back",
         "single-item recall", "verbal")
obe <- c("set size", "recall accuracy", "WM updating",
         "random order recall", "digits")
ada <- c("set size", "accuracy", "change detection",
         "change detection", "visual (colored squares)")

d1.1 <- data.frame(x,uns, bun, mce, jon, ver, obe, ada)

names(d1.1) <- datNames

benchmark1.1 <- d1.1

save(benchmark1.1, file = "./pkg/data/benchmark1.1.rda", compress = "xz")



