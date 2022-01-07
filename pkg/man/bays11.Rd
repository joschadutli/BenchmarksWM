\name{bays11}
\alias{bays11}
\docType{data}
\title{
  Bays, Gorgoraptis, Wee, Marshall, & Husain's (2011) Data on the Effect
  of Presentation Duration in Visual Working Memory
}
\description{
  Individual-trial data from Experiment 1 in Bays et al. (2011), showing the 
  effect of presentation duration and set size in a visual Working Memory task.
  The data show a performance increases for longer presentation durations in a
  range of 25 ms to about 100 ms. Longer presentation times don't result in
  better performance compared to the performance at 100 ms.
}
\usage{data("bays11")}
\format{
  A data frame with 25599 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{size}}{Set size: \code{1, 2, 4, 6} (between-subjects)}
    \item{\code{ptime}}{Presentation time for the array of colored orientations in seconds (within-subjects): \cr
    Levels: \code{25 ms},\code{50 ms}, \code{75 ms},
    \code{100 ms}, \code{125 ms}, \code{300 ms},
   \code{500 ms}, \code{1000 ms}
    }
    \item{\code{error}}{Precision error in degrees}
    \item{\code{dev}}{Precision error in radians}
  }
}
\details{
  These are the individual-trial data collected and analyzed by Bays et al. 
  (2011, Exp. 1), listed as a benchmark dataset in Oberauer et al. (2018) and
  archived by Oberauer (2019). Thirty-two participants were assigned to four 
  groups of eight. Each group was presented with one of the four set size levels
  throughout the 800 trials of the study. Stimuli were colored bars and 
  participants had to recall the orientation of a presented bar that was randomly
  selected. Stimuli were presented simultaneously. Presentation duration was 
  manipulated within-subjects.
}
\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Bays, P. M., Gorgoraptis, N., Wee, N., Marshall, L., & Husain, M. (2011).
  Temporal dynamics of encoding, storage, and reallocation of visual working 
  memory. \emph{Journal of Vision, 11}, 1-15. http://dx.doi.org/10.1167/11.10.6
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
data(bays11)
## Reproduce Figure 6B in Oberauer et al. (2018)
library("circular")
### get circular mean (sd) in rad
pd <- aggregate(dev ~ ptime*size, data = bays11, FUN = sd.circular)
pd$devrad <- 1/pd$dev
plot(c(0.0,1.0), c(0,6), type = "n", xlab = "Presentation Time (s)",
     ylab = "Mean Precision (1/rad)", main = "Reproduction of Colors", xaxt = "n")
axis(side = 1, at = c(0,0.2,0.4,0.6,0.8,1.0), labels = TRUE)
lines(x = pd$ptime[pd$size == 1], y = pd$devrad[pd$size == 1], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$ptime[pd$size == 2], y = pd$devrad[pd$size == 2], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$ptime[pd$size == 4], y = pd$devrad[pd$size == 4], 
      type = "b", lty = 3, pch = 17)
lines(x = pd$ptime[pd$size == 6], y = pd$devrad[pd$size == 6], 
      type = "b", lty = 4, pch = 18)
legend(1.0, 0, c("1", "2", "3", "4"), lty = 1:4, pch=15:18, title = "Set size:",
       horiz = TRUE, cex = 0.6, yjust = 0, xjust = 1)

}
\keyword{datasets}
