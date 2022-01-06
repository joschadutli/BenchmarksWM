\name{nimmo06}
\alias{nimmo06}
\docType{data}
\title{
  Nimmo & Lewandowsky's (2006) Data on the Absence of the Temporal Isolation Effect in 
  Forward Serial Recall
}
\description{
  These are the individual-trial data from Experiment 2 in Nimmo and Lewandowsky (2006). 
  Presentation modality (visual vs. auditory) was manipulated between groups. Items were
  temporally isolated but presentation schedules were unpredictable. The data show no effect
  of temporal isolation.
}
\usage{data("nimmo06")}
\format{
  A data frame with 50400 observations on the following 9 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Condition: \code{visual} or \code{auditory}}
    \item{\code{serpos}}{Serial Position (1-7)}
    \item{\code{item}}{Memory Item}
    \item{\code{postitem.interval}}{Post-item Interval (s)}
    \item{\code{response}}{Response}
    \item{\code{rt}}{Response times (s)}
    \item{\code{correct}}{Correct serial recall (1/0)}
  }
}
\details{
  For further details, see Nimmo and Lewandowsky (2006).
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Nimmo, L. M., & Lewandowsky, S. (2006). Distinctiveness revisited: Unpredictable 
  temporal isolation does not benefit short-term serial recall of heard or seen events. 
  \emph{Memory & Cognition, 34}, 1368-1375. http://dx.doi.org/10.3758/BF03193278
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 3 in Nimmo & Lewandowsky (2006)
data(nimmo06)
pd <- aggregate(correct ~ serpos+condition, data = nimmo06, FUN = mean)
plot(c(1,7), c(0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", 
     main = "Serial Position Curves", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7), labels = unique(pd$serpos), 
     cex.axis = 1.0)
lines(x = pd$serpos[pd$condition == "visual"], 
      y = pd$correct[pd$condition == "visual"], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$serpos[pd$condition == "auditory"], 
      y = pd$correct[pd$condition == "auditory"], 
      type = "b", lty = 2, pch = 16, col = "darkgrey")
legend(1, 0, c("visual", "auditory"), 
       lty = 1:2, 
       pch=15:16, col = c("black","darkgrey"), 
       horiz = F, cex = 0.9, yjust = 0, xjust = 0)
}
\keyword{datasets}
