\name{quinlan17}
\alias{quinlan17}
\docType{data}
\title{
  Quinlan, Roodenrys, & Miller's (2017) Data on the Word Frequency Effect in Serial Recall 
  and Serial Reconstruction
}
\description{
  Individual-trial data from Experiment 4 in Quinlan, Roodenrys, and Miller (2017). 
  Participants either performed a serial reconstruction or a serial recall task on lists of
  high-frequency or low-frequency words. Both conditions were randomized over trials in 
  order to eliminate predictability. The data show an effect effect of word frequency in 
  both tests.
}
\usage{data("quinlan17")}
\format{
  A data frame with 32760 observations on the following 9 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Memory Task: serial \code{recall} or serial \code{reconstruction}}
    \item{\code{frequency}}{Word Frequency: \code{high} or \code{low}}
    \item{\code{condition}}{Experimental condition (combination of \code{task} and \code{frequency}): \code{Hi-Recall}, \code{Hi-Recon}, \code{Lo-Recall}, or \code{Lo-Recon}}
    \item{\code{serpos}}{Serial Position (1-6)}
    \item{\code{stim}}{Presented Stimuli}
    \item{\code{resp}}{Responses}
    \item{\code{acc}}{Accuracy (1/0)}
  }
}
\details{
  For further details, see Experiment 4 in Quinlan, Roodenrys, and Miller (2017).
}
\source{
   Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Quinlan, P.T., Roodenrys, S. & Miller, L.M. (2017). Serial reconstruction of order and
  serial recall in verbal short-term memory. \emph{Memory & Cognition, 45}(7), 1126–1143. 
  https://doi.org/10.3758/s13421-017-0719-y
  
  
}
\examples{
### Reconstruct Figure 4 in Quinlan, Roodenrys, & Miller (2017)
data(quinlan17)
pd <- aggregate(acc ~ serpos+condition, data = quinlan17, FUN = mean)
plot(c(1,6), c(0,1.0), type = "n", xlab = "Set Size",
     ylab = "Proportion correct", main = "Word Frequency Effect in Serial Recall
     and Serial Reconstruction", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = c(1,2,3,4,5,6), 
     cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "Hi-Recall"], 
      y = pd$acc[pd$condition == "Hi-Recall"], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$serpos[pd$condition == "Lo-Recall"], 
      y = pd$acc[pd$condition == "Lo-Recall"], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$serpos[pd$condition == "Hi-Recon"], 
      y = pd$acc[pd$condition == "Hi-Recon"], 
      type = "b", lty = 3, pch = 17)
lines(x = pd$serpos[pd$condition == "Lo-Recon"], 
      y = pd$acc[pd$condition == "Lo-Recon"], 
      type = "b", lty = 4, pch = 18)
legend(1, 0, c("Hi-Recall", "Lo-Recall", "Hi-Recon", "Lo-Recon"), lty = 1:4, 
       pch=15:18, 
       horiz = F, cex = 0.6, yjust = 0, xjust = 0)
}
\keyword{datasets}
