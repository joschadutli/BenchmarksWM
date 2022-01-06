\name{ricker14}
\alias{ricker14}
\docType{data}
\title{
  Ricker, Spiegel, & Cowan's (2014) Data on Time-Based Forgetting in visual Working Memory
}
\description{
  Individual-trial data from Experiment 1 in Ricker, Spiegel, & Cowan (2014). Three levels
  of Retention Interval (RI) and three levels of Inter-trial Interval (ITI) were combined in
  the experiment. The data show a main effect of RI but no interaction with ITI, suggesting
  that time-based forgetting in visual WM is a function of decay rather than temporal
  distinctiveness. Memory was tested with a Change Detection task.
}
\usage{data("ricker14")}
\format{
  A data frame with 9452 observations on the following 9 variables.
  \describe{
    \item{\code{subject}}{Unique subject ID for each participant}
    \item{\code{RI}}{Retention Interval: 1s, 6s or 12s}
    \item{\code{ITI}}{Inter-trial Interval: 1s, 6s or 12s}
    \item{\code{accuracy}}{Correct/incorrect: 1/0}
    \item{\code{RT}}{Response Time (ms)}
    \item{\code{corr_resp}}{Correct response: \code{d} = different, \code{s} = same}
    \item{\code{resp}}{Response: \code{d} = different, \code{s} = same}
    \item{\code{setsize}}{Set size: 3 in all trials}
    \item{\code{ptrl}}{Accuracy on previous trial: \code{cor} = correct, \code{inc} = incorrect}
  }
}
\details{
   For details, see: Experiment 1 in Ricker et al. (2014).
}
\source{
   
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Ricker, T. J., Spiegel, L. R., & Cowan, N. (2014). Time-based loss in visual short-term
  memory is from trace decay, not temporal distinctiveness. \emph{Journal of Experimental
  Psychology: Learning, Memory, and Cognition, 40}, 1510-1523. 
  http://dx.doi.org/10.1037/xlm0000018
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
data(ricker14)
### Reconstruct Figure 2 from Ricker et al. (2014)
pd <- aggregate(accuracy ~ RI*ITI, data = ricker14, FUN = mean)
plot(c(0.0,12), c(0.6,0.9), type = "n", xlab = "Retention Interval (s)",
     ylab = "Proportion correct", main = "Figure 2 in Ricker et al. (2014)", xaxt = "n")
axis(side = 1, at = c(1,6,12), labels = T)
lines(x = pd$RI[pd$ITI == 1], y = pd$accuracy[pd$ITI == 1], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$RI[pd$ITI == 6], y = pd$accuracy[pd$ITI == 6], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$RI[pd$ITI == 12], y = pd$accuracy[pd$ITI == 12], 
      type = "b", lty = 3, pch = 17)
legend(12, 0.9, c("1s", "6s", "12s"), lty = 1:3, pch=15:17, 
       title = "Inter-trial Interval (s):",
       horiz = T, cex = 0.6, yjust = 1, xjust = 1)

}
\keyword{datasets}
