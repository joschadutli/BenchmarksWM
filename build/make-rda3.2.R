## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM2.1 #####################
pth  <- "BenchmarksWM.Data/BM3.2.SP.Modality/"

fnam <- paste0(pth, "Harvey.Beaman.2007.xls")
HB <- read_excel(fnam,sheet="SPSS Final", col_names=TRUE)
HB$Response <- as.numeric(HB$Response)
HB$S <- as.numeric(HB$S)
aw <- as.data.frame(HB[1:51, 1:which(letters=="k")]) #ridiculously, although aw is allegedly a data frame, it is in fact a tibble that has to be converted into a data frame!
an <- as.data.frame(HB[1:51, c(1:2, which(letters=="u"):(which(letters=="u")+8))])
vw <- as.data.frame(HB[54:104, 1:which(letters=="k")])
vn <- as.data.frame(HB[54:104, c(1:2, which(letters=="u"):(which(letters=="u")+8))])

#function that does rearranging
convert <- function(aw,mod,mat) {
  convert <- aw %>% gather("spos","rec",contains("-B")) %>% 
    mutate(spos=as.numeric(substr(spos,5,5))) %>%
    mutate(modality=(mod)) %>%
    mutate(material=(mat))
}
#now gather columns in the usual way using function
aw2 <- convert(aw,"auditory","words")
an2 <- convert(an,"auditory","numbers")
vw2 <- convert(vw,"visual","words")
vn2 <- convert(vn,"visual","numbers")
#harvbeaman <- list(aw2,an2,vw2,vn2)
harvbeaman <- bind_rows(aw2,an2,vw2,vn2) %>% mutate(modality=as.factor(modality)) %>% 
  mutate(material=as.factor(material)) %>% mutate(rec=as.numeric(rec))


harvey07 <- harvbeaman %>% dplyr::rename(subj = S) %>% 
  dplyr::rename(r_mode = Response) %>% dplyr::rename(serpos = spos)
harvey07$acc <- harvey07$rec/100

harvey07$r_mode[harvey07$r_mode == 1] <- "written"
harvey07$r_mode[harvey07$r_mode == 2] <- "spoken"

harvey07 <- harvey07 %>% select(subj, r_mode, modality, material, serpos, acc)
save(harvey07, file="./pkg/data/harvey07.rda")

## Reproduce Figure 8 in Oberauer et al. (2018)

d_writ <- harvey07[which(harvey07$r_mode == "written"),]
d_spok <- harvey07[which(harvey07$r_mode == "spoken"),]

par(mfrow=c(2,1))

pdw <- aggregate(acc ~ serpos*modality*material, data = d_writ, FUN = mean)

plot(c(1,9), c(0.0,1.05), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", 
     main = "Written response", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = T)

lines(x = pdw$serpos[pdw$modality == "auditory" & pdw$material == "numbers"], 
      y = pdw$acc[pdw$modality == "auditory" & pdw$material == "numbers"], 
      type = "b", lty = 1, pch= 21, bg="black", col="black")
lines(x = pdw$serpos[pdw$modality == "auditory" & pdw$material == "words"], 
      y = pdw$acc[pdw$modality == "auditory" & pdw$material == "words"], 
      type = "b", lty = 2, pch= 22, bg="black", col="black")
lines(x = pdw$serpos[pdw$modality == "visual" & pdw$material == "numbers"], 
      y = pdw$acc[pdw$modality == "visual" & pdw$material == "numbers"], 
      type = "b", lty = 3, pch= 21, bg="grey", col="black")
lines(x = pdw$serpos[pdw$modality == "visual" & pdw$material == "words"], 
      y = pdw$acc[pdw$modality == "visual" & pdw$material == "words"], 
      type = "b", lty = 4, pch= 22, bg="grey", col="black")


### Figure 8b: Spoken response
pds <- aggregate(acc ~ serpos*modality*material, data = d_spok, FUN = mean)
plot(c(1,9), c(0.0,1.05), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", 
     main = "Spoken response", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = T)

lines(x = pds$serpos[pds$modality == "auditory" & pds$material == "numbers"], 
      y = pds$acc[pds$modality == "auditory" & pds$material == "numbers"], 
      type = "b", lty = 1, pch= 21, bg="black", col="black")
lines(x = pds$serpos[pds$modality == "auditory" & pds$material == "words"], 
      y = pds$acc[pds$modality == "auditory" & pds$material == "words"], 
      type = "b", lty = 2, pch= 22, bg="black", col="black")
lines(x = pds$serpos[pds$modality == "visual" & pds$material == "numbers"], 
      y = pds$acc[pds$modality == "visual" & pds$material == "numbers"], 
      type = "b", lty = 3, pch= 21, bg="grey", col="black")
lines(x = pds$serpos[pds$modality == "visual" & pds$material == "words"], 
      y = pds$acc[pds$modality == "visual" & pds$material == "words"], 
      type = "b", lty = 4, pch= 22, bg="grey", col="black")
legend(4.5, 1.0, c("Auditory — numbers", "Auditory — words", "Visual — numbers", 
                   "Visual — words"), lty = 1:4, pch = c(21,22,21,22),
       pt.bg = c("black","black","grey","grey"), horiz = F, cex = 0.6, 
       yjust = 1, xjust = 0)






