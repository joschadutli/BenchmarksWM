## Convert data sets to R package format (.rda)
## Author: Joscha Dutli
## Licence: GPL 2.0+
library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
############### BM4.1 #####################
pth  <- "BenchmarksWM.Data/BM4.1.1.LocalityConstraint/"

## farrell04 again.. maybe don't read that one in, but add some description to the Rd file that you already have. Also, mention the dataset in the BM4 description

### Reproduce Figure B2 in Oberauer et al. (2018) without CIs
data(farrell04)
fl04m     <- aggregate(rt~condition+spos, data=farrell04, FUN=mean)
par(mfrow=c(1,1))  #accuracy and then RT
bgk <- c("gray","black")
ltyk <- c("solid","dashed")

#now plot RT by serial position
plot(c(0,7),c(0,3000), xlim=c(0.5,6.5),ylim=c(0,3100), type="n", las=1,
     xlab="Serial Position", ylab="Mean Latency (ms)",cex.lab=1.2,cex.axis=1.)
for (k in c(0:1)) {
  tbp <- fl04m[which(fl04m$condition==k),]
  xx <- tbp$spos - 0.05 + k*0.1
  lines(xx,tbp$rt,lwd=2,lty=ltyk[k+1])
  points(xx,tbp$rt,pch=21+k,bg=bgk[k+1],cex=1.5)
}
legend(6,2900,c("No interference","Interference"),lty=ltyk,pch=20+c(1:2),pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=1)



fl04 <- read.table(paste0(pth, "FL04_2.dat"),header=FALSE)
names(fl04) <- c("subject", "trial", "condition",paste("opp",c(1:6),sep=""),paste("rt",c(1:6),sep=""))
#The columns are subject, trial, condition  (0=no  interference, 1 = interference), 
#the recall order (columns are output positions), and the recall latencies
#intrusions prevented, omissions coded as -9
Transgrad <- function(data) {
  tdist <- matrix(0,1,length(data))
  topp <- matrix(0,1, length(data))
  for (sp in 1:length(data)) {
    if (data[sp] > 0) {
      td <- abs(data[sp]-sp)
      tdopp <- abs(1:length(data)-sp)
      tdist[td] <- tdist[td] + 1
      topp[tdopp[tdopp>0]] <- topp[tdopp[tdopp>0]] + 1
    }
  }
  return(cbind(tdist, topp))
}
Outdat <- fl04[,which(names(fl04)=="opp1"):which(names(fl04)=="opp6")]
TD <- as.data.frame(t(apply(Outdat, MARGIN=1, FUN=Transgrad)))
names(TD) <- c("td1", "td2", "td3", "td4", "td5", "td6", "tdopp1", "tdopp2", "tdopp3", "tdopp4", "tdopp5", "tdopp6")
fl04td <- cbind(fl04, TD)
fl04tdagg    <- aggregate(cbind(td1, td2, td3, td4, td5, td6) ~ subject+condition, data=fl04td, FUN=mean)
fl04tdoppagg <- aggregate(cbind(tdopp1, tdopp2, tdopp3, tdopp4, tdopp5, tdopp6) ~ subject+condition, data=fl04td,  FUN=mean)
for (sp in 1:5) {
  tdname <- paste("td", as.character(sp), sep="")
  tdoppname <- paste("tdopp", as.character(sp), sep="")
  fl04tdagg[,tdname] <- fl04tdagg[,tdname]/fl04tdoppagg[,tdoppname]
}




###### start over with Rerko et al.: spatial gradient, (2014)

rm(list=ls())
############### BM2.1 #####################
pth  <- "BenchmarksWM.Data/BM4.1.1.LocalityConstraint/"

## Rerko et al spatial gradient
spatgrad <- read.table(paste0(pth,"spatialGradientSerial.txt"), header=F)
spatgrad <- spatgrad[,c(1:3,6:10,45:49,50)]
names(spatgrad) <- c("id", "session", "trial", "color1", "color2", "color3", "color4", "color5",
                     "dist1", "dist2", "dist3", "dist4", "dist5", "response")
selectedIdx <- apply(spatgrad[,c(4:8,14)], MARGIN=1, FUN=function(x){which(as.numeric(x[1:5])==as.numeric(x[6]))}) #which color is equal to the response
spatgrad$selectedIdx <- as.numeric(selectedIdx)
correctIdx <- apply(spatgrad[,c(9:13)], MARGIN = 1, FUN=function(x){which(as.numeric(x[1:5])==0.0)})
spatgrad$correctIdx <- as.numeric(correctIdx)
selectedDist <- apply(spatgrad[,c(9:13, 15)], MARGIN=1, FUN=function(x){x[x[6]]}) #which distance is the selected color
spatgrad$selectedDist <- as.numeric(selectedDist)

subjVec <- sort(unique(spatgrad$id))
sessionVec <- sort(unique(spatgrad$session))
trialVec <- sort(unique(spatgrad$trial))
check <- length(subjVec)*length(sessionVec)*length(trialVec)

for (sj in subjVec) {
  for (ss in sessionVec) {
    for (tr in trialVec) {
      distVec <- grepl("dist", names(spatgrad))
      
    }
  }
}

spatgradError <- subset(spatgrad, selectedDist > 0) #keep only distances > 0 (i.e., errors)

# baseline: draw from the possible errors at random
errorDist <- as.data.frame(t(apply(spatgrad[,c(9:13)], MARGIN=1, FUN=function(x){x[x>0]})))  #keep the 4 distances that are not 0
distSample <- NULL
nSamples <- 100
for (sample in 1:nSamples) {
  dsample <- apply(errorDist, MARGIN=1, FUN=function(x){x[sample(1:4,1)]})  # sample from the 4 distances at random -> pick error response at random
  distSample <- c(distSample, dsample)
}
nBins <- 6
histbounds <- as.numeric(quantile(distSample, probs=seq(0, 1, 1/nBins))) # histogram bounds selected so that random sampling results in equal frequencies
idvector <- unique(spatgrad$id)
densities <- matrix(NA, length(idvector), nBins)
for (subj in idvector) {
  dd <- subset(spatgradError, id==subj)
  h <- hist(dd$selectedDist, breaks=histbounds, plot=F)
  densities[subj,] <- as.numeric(h$density)
}

library(Hmisc)

source(paste(dirname(getwd()), "/BenchmarksWM-joscha/BenchmarksWM.Data/Functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/BenchmarksWM-joscha/BenchmarksWM.Data/Functions/Bakeman.R", sep=""))

Dens <- Confint(Bakeman(densities))
binwidths <- diff(histbounds)
bincenters <- histbounds[1:nBins] + 0.5*binwidths
errbar(bincenters, y=Dens[1,], yplus=Dens[2,], yminus=Dens[3,], 
       xlab="Euclidean Distance", ylab="P(Selection|Error)", 
       xlim=c(0.5,max(histbounds)+0.5), ylim=c(0,0.3), add=F, type="b", cex=1.3)
xcoord <- cbind(histbounds,histbounds)
ycoord <- cbind(rep(0,nBins+1), rep(0.05,nBins+1))
for (lin in 1:dim(xcoord)[1]) lines(x=xcoord[lin,], y=ycoord[lin,], col="red")


names(spatgrad)[which(names(spatgrad) == "selectedIdx")] = "selColor"
names(spatgrad)[which(names(spatgrad) == "correctIdx")] = "corrColor"
names(spatgrad)[which(names(spatgrad) == "selectedDist")] = "dist"

spatgrad$correct[spatgrad$selColor == spatgrad$corrColor] <- 1
spatgrad$correct[spatgrad$selColor != spatgrad$corrColor] <- 0


rerko14 <- spatgrad %>% select(id,session,trial,dist1:dist5,dist,correct)

save(rerko14, file="./pkg/data/rerko14.rda")

pd <- rerko14[which(rerko14$correct == 0),]
hist(pd$dist, breaks=10, xlab = "Euclidean distance", main = "Prob. for Spatial Distances of Errors", freq=F)



##################
### BM 4.1.2 Fill in effect

##################### Read 19 data sets of serial-recall tests compiled by Farrell, Hurlstone, & Lewandowsky (2013, Memory & Cognition) for analysis of fill-in vs. in-fill errors #######

# each line is one trial; successive columns represent successive outputs, coding the input position of the item recalled 
# in that output; -1 = omission, -9 = extralist intrusion

rm(list=ls())

fnam <- 'Farrell_Lewandowsky_2003_E1.dat'

Dat <- read.table(fnam, header=F)
names(Dat)[1] <- "id"
if (dim(Dat)[2] < 8) {
  for (col in 1:(8-dim(Dat)[2])) {
    colVec <- rep(NA,dim(Dat)[1])
    Dat <- cbind(Dat, colVec)
  }
}
for (serpos in 1:(dim(Dat)[2]-1)) names(Dat)[serpos+1] <- paste0("serpos", serpos)
Dat$exp_name <- 'Farrell_Lewandowsky_2003_E1'
Dat$exp_code = 'a'

# experiment names
Ename = c('Farrell_Lewandowsky_2003_E3', 
          'Farrell_Lewandowsky_2004_E1',
          'Nimmo_Lewandowsky_2006_E1',
          'Nimmo_Lewandowsky_2006_E2_aud',
          'Nimmo_Lewandowsky_2006_E2_vis',
          'Lewandowsky_Brown_Wright_Nimmo_2006_E1_quiet',
          'Lewandowsky_Brown_Wright_Nimmo_2006_E1_suppr',     
          'Lewandowsky_Geiger_Oberauer_2008_E1',
          'Lewandowsky_Geiger_Oberauer_2008_E2',
          'Lewandowsky_Geiger_Oberauer_2008_E3',
          'Lewandowsky_Geiger_Oberauer_2008_E4',
          'Lewandowsky_Farrell_2008_E2',
          'Farrell_2008_E1',
          'Farrell_2008_E2',
          'Lewandowsky_Geiger_Morrell_Oberauer_2010_E1',
          'Lewandowsky_Geiger_Morrell_Oberauer_2010_E2',
          'Lewandowsky_Geiger_Morrell_Oberauer_2010_E3',
          'Farrell_Lewandowsky_inpress_E1',
          'Farrell_Lewandowsky_inpress_E2',
          'Farrell_Lewandowsky_inpress_E3')

Ecode <- c('b',
           'c',
           'd',
           'e',
           'f',
           'g',
           'h',
           'i',
           'j',
           'k',
           'l',
           'm',
           'n',
           'o',
           'p',
           'q',
           'r',
           's',
           't',
           'u')



for (experiment in 1:length(Ename)) {
  filename <- paste0(Ename[experiment], '.dat')
  data <- read.table(filename, header=F)
  names(data)[1] <- "id"
  if (dim(data)[2] < 8) {
    for (col in 1:(8-dim(data)[2])) {
      colVec <- rep(NA,dim(data)[1])
      data <- cbind(data, colVec)
    }
  }
  for (serpos in 1:7) names(data)[serpos+1] <- paste0("serpos", serpos)
  data$exp_name <- Ename[experiment]
  data$exp_code <- Ecode[experiment]
  Dat <- rbind(Dat,data)
}

Datlong <- gather(Dat,"serpos","outpos",2:8)

farrell13 <- Datlong %>% mutate(serpos=as.numeric(substr(serpos,7,8)))
farrell13$outpos[farrell13$exp_code == 'a' & farrell13$outpos == 0] <- -1

corrVec <- unique(farrell13$outpos)
farrell13$correct <- 1
farrell13$correct[farrell13$outpos < 0] <- 0
farrell13$correct[is.na(farrell13$outpos)] <- 0

farrell13 <- farrell13 %>% select(exp_code, exp_name, id, serpos, outpos, correct)

save(farrell13, file = "./pkg/data/farrell13.rda")


#####################
### BM 4.4 Ranschburg

rm(list = ls())
pth  <- "BenchmarksWM.Data/BM4.4.Ranschburg/"
library(readxl)

d1 <- read_excel(paste0(pth, "Henson_JEPLMC_98_Exp1_ItemScoring.xls"))

######################
### BM4.5 Continuous Errors

rm(list = ls())
pth  <- "BenchmarksWM.Data/BM4.5.ErrorDistributionDE/"

# Read color DE data, simple version (used for plot in the Benchmarks paper)
de <- read.table(paste0(pth, "WeijiDelayedEstimation.dat"), header=F)
names(de) <- c("id", "setsize", "error")
de$error_rad <- pi*de$error/180

# compute trials:
sum(de$id[de$id == 3])/3
idVec <- sort(unique(de$id))

trVec <- c()
for (subj in idVec) {
  itrVec <- 1:432
  trVec <- rbind(trVec, itrVec)
}
length(trVec)

detr <- cbind(de, trVec)



Color <- read.table(paste0(pth,"vandenberg_et_al_2012_color_long.dat"), header=F)
Orient <- read.table(paste0(pth,"vandenberg_et_al_2012_orient_long.dat"), header=F)
names(Color) <- c("id", "trial", "exp", "setsize", 
                  "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                  "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8",
                  "response")
names(Orient) <- c("id", "trial", "exp", "setsize", 
                   "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                   "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8",
                   "response")
# stim1 to stim8 are the angles (1 to 360) of the stimuli (in case of colors: angles in the color wheel). The experiments distinguished only 180 angles, so the angles were doubled to scale them into a range from 1 to 360 (degrees) in steps of 2.
# loc1 to loc8 refer to the 8 possible locations of stimuli in the array. 
# stim1 is the target stimulus, and loc1 is the target location
# stimuli that did not exist (set sizes < 8) are set to 360; locations that did not exist are set to 0. 
# In Color, exp=3 is the experiment using the color wheel, and exp=4 is the experiment using scrolling. In Orient, this variable is meaningless. 

# Paul Bays' wrap function -> signed angular difference (radians)!
wrap = function(angle) {
  wangle <- ( (angle + pi) %% (2*pi) ) - pi
  return(wangle)
}


Color$targetrad <- pi*Color$stim1/180
Color$responserad <- pi*Color$response/180
Color$devrad <- wrap(Color$responserad - Color$targetrad)
Color$errorrad <- abs(Color$devrad)

Orient$targetrad <- pi*Orient$stim1/180
Orient$responserad <- pi*Orient$response/180
Orient$devrad <- wrap(Orient$responserad - Orient$targetrad)
Orient$errorrad <- abs(Orient$devrad)

## name experiments as convenient as possible
## exp3 colorwheel, exp4 scrolling colors, exp5 orientations = exp2 in PNAS

Color$devrad[Color$exp == 3 & Color$id == 2 & Color$trial == 4]
Color$devrad[Color$exp == 4 & Color$id == 2 & Color$trial == 4]

Color$stims <- "colors"
Orient$stims <- "orientations"
Color$response_selection[Color$exp == 3] <- "colorwheel"
Color$response_selection[Color$exp == 4] <- "scrolling"
Orient$response_selection <- "rotation"

Color$experiment[Color$exp == 3] <- "ExpS3"
Color$experiment[Color$exp == 4] <- "Exp1"
Orient$experiment <- "Exp2"

vandenberg <- Color %>% select(experiment, id, trial, stims, response_selection,
                               setsize, stim1, response, targetrad, responserad, devrad,
                               errorrad)
vandenberg_o <- Orient %>% select(experiment, id, trial, stims, response_selection,
                               setsize, stim1, response, targetrad, responserad, devrad,
                               errorrad)
vandenberg12 <- rbind(vandenberg, vandenberg_o)

vandenberg12 <- vandenberg12 %>% rename(target = stim1)

save(vandenberg12, file="./pkg/data/vandenberg12.rda")

### Approximate reproduction of Figure 11 in Oberauer et al. (2018)
pd <- vandenberg12[which(vandenberg12$experiment == "ExpS3" & vandenberg12$setsize %in% 
                           c(1,2,4,8)),]
nbins <- 21
breakpoints <- seq(-pi, pi, length.out=nbins+1)
idvector <- unique(pd$id)
ssvector <- sort(unique(pd$setsize))
errorFreq <- array(dim=c(length(ssvector), nbins))

for (ss in 1:length(ssvector)) {
  d <- subset(pd, setsize==ssvector[ss])
  h <- hist(d$devrad, breaks=breakpoints, plot=F)
  errorFreq[ss,] <- h$counts/length(idvector)
}

plot(c(-3,3), c(0.0,60), type = "n", xlab = "Error (rad)",
     ylab = "Mean Frequency", main = "Errors in continuous reproduction", xaxt = "n")
axis(side = 1, at = c(-3,-2,-1,0,1,2,3), labels = T)
lines(x = h$mids, y = errorFreq[1,], 
      type = "b", lty = 1, pch = 15)
lines(x = h$mids, y = errorFreq[2,], 
      type = "b", lty = 2, pch = 16)
lines(x = h$mids, y = errorFreq[3,], 
      type = "b", lty = 3, pch = 17)
lines(x = h$mids, y = errorFreq[4,], 
      type = "b", lty = 4, pch = 18)
legend(3.0, 60, c("N=1", "N=2", "N=4", "N=8"), lty = 1:4, pch=15:18, title = "Set size:",
       horiz = F, cex = 0.6, yjust = 1, xjust = 1)
