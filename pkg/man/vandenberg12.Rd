\name{vandenberg12}
\alias{vandenberg12}
\docType{data}
\title{
  Data on Error Distributions on Continuous Response Scales from Van den Berg et al. (2012)
}
\description{
  Individual-trial data from three experiments conducted by Van den Berg et al. (2012). The 
  experiments tested memory for one item on a continuous scale in visual WM (either colors
  or orientations). The data show that error distributions are wider at higher set sizes. 
}
\usage{data("vandenberg12")}
\format{
  A data frame with 37824 observations on the following 12 variables.
  \describe{
    \item{\code{experiment}}{
    \itemize{\item{\code{"Exp1"}: Experiment 1 in Van den Berg et al. (2012)}
    \item{\code{"Exp2"}: Experiment 2 in Van den Berg et al. (2012)}
    \item{\code{"ExpS3"}: Experiment reported in the Supplementary Materials in Figure S3 of Vandenberg et al. (2012)}}}
    \item{\code{id}}{Unique subject ID for each participant within one experiment.}
    \item{\code{trial}}{Unique trial ID within each participant.}
    \item{\code{stims}}{Stimulus Material: \itemize{
    \item{\code{colors} in Exp1 & ExpS3}
    \item{\code{orientations} in Exp2}}}
    \item{\code{response_selection}}{Type of Response Selection: \itemize{
    \item{\code{scrolling} in Exp1}
    \item{\code{rotation} in Exp2}
    \item{\code{colorwheel} in ExpS3}}}
    \item{\code{setsize}}{Set size: 1-8}
    \item{\code{target}}{Target color or orientation in degrees}
    \item{\code{response}}{Response in degrees}
    \item{\code{targetrad}}{Target color or orientation in radians}
    \item{\code{responserad}}{Response in radians}
    \item{\code{devrad}}{Deviation in radians (range: [-pi, pi])}
    \item{\code{errorrad}}{Error (absolute) in radians (range: [0, pi])}
  }
}
\details{
  In Exp1 (Experiment 1 in Van den Berg et al. (2012)), stimuli were colors and responses were
  selected by scrolling through all possible colors. In Exp2 (Experiment 2), stimuli were 
  oriented Gabor patches and responses were given by rotating a Gabor patch into the correct
  orientation. In ExpS3 that can be found in the online supplementary materials provided by
  Van den Berg et al. (2012), stimuli were colors and responses were selected from a color
  wheel. For further details, see Van den Berg et al. (2012).
}
\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}
\references{
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  van den Berg, R., Shin, H., Chou, W.-C., George, R., & Ma, W. J. (2012). Variability in
  encoding precision accounts for visual short-term memory limitations. \emph{Proceedings of
  the National Academy of Sciences of the United States of America, 109}, 8780–8785. 
  http://dx.doi.org/10.1073/pnas.1117465109
  
}
\examples{
### Approximate reproduction of Figure 11 in Oberauer et al. (2018) (Data from ExpS3)
data(vandenberg12)
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
}
\keyword{datasets}
