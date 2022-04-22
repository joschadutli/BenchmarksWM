\name{souza14}
\alias{souza14}
\docType{data}
\title{
  Souza et al. (2014) Data on the Retro Cue Effect:
  Attending to One Item Frees Capacity
}
\description{
  The data show the retro-cue effect. The effect is best observed at
  large set sizes and with a long cue-target interval (CTI, i.e., post-
  cue interval).
}
\usage{data("souza14")}
\format{
  A data frame with 30720 observations on the following 9 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Refers to the cue-target interval}
    \item{\code{CTI}}{Cue-target interval}
    \item{\code{setsize}}{Set size}
    \item{\code{probetype}}{Probe type at test: match, intrusion or new probe}
    \item{\code{acc}}{Accuracy}
    \item{\code{rt}}{Response time (s)}
  }
}
\details{
  During the retention interval of a working memory task, presenting a
  retro-cue directs attention to 1 of the items in working memory.
  Testing the cued item leads to faster and more accurate responses.
  For further details, see Souza et al. (2014).
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

  Souza, A. S., Rerko, L., & Oberauer, K. (2014). Unloading and
  reloading working memory: Attending to one item frees
  capacity. \emph{Journal of Experimental Psychology: Human
  Perception and Performance, 40}, 1237-1256.
  http://dx.doi.org/10.1037/a0036331

}
\examples{
data(souza14)
pd <- aggregate(acc ~ condition + setsize, data = souza14, FUN = mean)
plot(c(1,6), c(0.7,1.0), type = "n", xlab = "Set Size",
     ylab = "Proportion correct",
     main = "Retro-Cue Effect in Change Detection", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = c(1,2,3,4,5,6),
     cex.axis = 0.7)
lines(x = pd$setsize[pd$condition == "no cue"],
      y = pd$acc[pd$condition == "no cue"],
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$setsize[pd$condition == "100 ms"],
      y = pd$acc[pd$condition == "100 ms"],
      type = "b", lty = 2, pch = 16, col = "lightgrey")
lines(x = pd$setsize[pd$condition == "400 ms"],
      y = pd$acc[pd$condition == "400 ms"],
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$setsize[pd$condition == "1000 ms"],
      y = pd$acc[pd$condition == "1000 ms"],
      type = "b", lty = 4, pch = 18, col = "darkgrey")
lines(x = pd$setsize[pd$condition == "2000 ms"],
      y = pd$acc[pd$condition == "2000 ms"],
      type = "b", lty = 5, pch = 19, col = "black")
legend(1, 0.7, c("no cue", "100 ms",  "400 ms","1000 ms", "2000 ms"),
lty = c(1:4,5),
       pch=15:19, col = c("black", "lightgrey", "grey",
       "darkgrey", "black"),
       horiz = F, cex = 0.6, yjust = 0, xjust = 0,
       title = "Post-cue interval:")

}
\keyword{datasets}
