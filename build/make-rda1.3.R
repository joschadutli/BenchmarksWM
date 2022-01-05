## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM1.3 #####################
pth  <- "BenchmarksWM.Data/BM1.3.NChunks/"

### Chen & Cowan (2009)

fnam = paste0(pth, "ChenCowan2009.xlsx")
data = read_excel(fnam)
data <- data %>% rename(subj = `Subject (not same Ss across groups despite numbers)`) %>% rename(group = `Group (2, no suppression; 4, suppression)`) %>% rename(`4p` = `4p_Chks`) %>% rename(`6p` = `6p_Chks`)

data$subj[data$group == 4] <- data$subj[data$group == 4] + 100
data$AS <- 0
data$AS[data$group == 2] <- "silent"
data$AS[data$group == 4] <- "AS"

long_data <- gather(data, key = "condition", value = "span", 3:12)

chen09 <- long_data
save(chen09, file="./pkg/data/chen09.rda")

### reproduce Figure 4 in Oberauer et al. (2018)
data("chen09")
dAS <- subset(chen09, group==4) # data where rehearsal is articulatory suppressed
plot_data <- aggregate(span ~ condition, data = dAS, FUN = mean)
plot_data <- plot_data[c(1,9,6,3,2,10,8,5,7,4),]
t <- as.table(plot_data$span)
names(t) <- plot_data$condition
barplot(t, main = "Figure 4: Numbers of Chunks recalled",
        xlab = "# = number of chunks presented; n = new single words; 
        s = pretrained single words; p = pretrained word pairs (chunks)", 
        ylab = "Number of chunks recalled", ylim = c(0,4))





