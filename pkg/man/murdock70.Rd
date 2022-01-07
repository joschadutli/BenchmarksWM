\name{murdock70}
\alias{murdock70}
\docType{data}
\title{
  Murdock and Okada's (1970) Data on Inter-Response Latency in Free Recall
}
\description{
  Individual-item data from Murdock and Okada (1970) showing an increase in
  inter-response latency with output position in free recall. The recall data
  show a primacy but most notably a recency effect. 
}
\usage{data("murdock70")}
\format{
  A data frame with 18720 observations on the following 6 variables.
  \describe{
    \item{\code{ID}}{Unique subject ID for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{output}}{Output Position: Ranging from 1 to 13.}
    \item{\code{input}}{Serial Position of recalled item: Rangeing from 1 to 20; 99 = extralist intrusion.}
    \item{\code{irl}}{Inter-response Latency measured in seconds}
    \item{\code{correct}}{Correct: Differentiates between correctly recalled items (
    \code{1} and extralist intrusions \code{0}}
  }
}

\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Murdock, B. B., & Okada, R. (1970). Interresponse times in single-trial free 
  recall. \emph{Journal of Experimental Psychology, 86}, 263-267. 
  http://dx.doi.org/10.1037/h0029993
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(murdock70)
## a plot with the serial recall curve and one for inter-response latency by output position:
## It is very much possible that the second plot is erronous!

## Serial Position Curve by Frequency
pd <- aggregate(correct ~ input, data = murdock70, FUN = function(x){sum(x)})
pd <- pd[which(pd$input != 0 & pd$input != 99), ]
plot(c(0,20),c(0,1300), xlim=c(1,20),ylim=c(0,1300), type="n", las=1,
     main = "Serial Position Curve by Frequency",
     xlab="Serial Position", ylab="Frequency of recall",cex.lab=1.2,cex.axis=1.)
lines(x = pd$input, 
      y = pd$correct, 
      type = "l", lty = 1)

## Inter-response Latency by Output Position
pdrt <- aggregate(irl ~ output, data = murdock70, FUN = mean)
plot(c(1,20),c(0,20), xlim=c(1,14),ylim=c(0,20), type="n", las=1,
     main = "Inter-response Latency by Output Position",
     xlab="Output Position", ylab="Inter-Response Latency",cex.lab=1.2,cex.axis=1.)
lines(x = pdrt$output, 
      y = pdrt$irl, 
      type = "l", lty = 1)
points(x = pdrt$output, 
       y = pdrt$irl, 
       pch = 22, bg = "black")
}
\keyword{datasets}
