\name{madigan71}
\alias{madigan71}
\docType{data}
\title{
  Magigan's (1971) serial position curves in forward and backward serial recall
}
\description{
  Madigan's (1971) data show serial position curves with primacy and recency effects
  for both forward and backward serial recall of words when presented visually.
  For details on the data for auditory presentation modality, see Madigan (1971).
}
\usage{data("madigan71")}
\format{
  A data frame with 256 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{Unique subject ID for each participant}
    \item{\code{modality}}{Presentation modality: \itemize{\item{1=audio}\item{2=visual}}}
    \item{\code{direction}}{Direction of serial recall: \itemize{\item{1=forward}\item{2=backward}}}
    \item{\code{serpos}}{Serial position}
    \item{\code{acc}}{Proportion correct (accuracy)}
  }
}

\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Madigan, S. A. (1971). Modality and recall order interactions in short-term memory
  for serial order. \emph{Journal of Experimental Psychology, 87}, 294-296.
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
data(madigan71)

# Reproduce Figure 7a in Obverauer et al. (2018) (data for visual presentation modality)
visual <- madigan71[which(magidan71$modality == 2),]

pd <- aggregate(acc ~ serpos*direction, data = visual, FUN = mean)

par(mfrow=c(1,2))
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", 
     main = "Serial Recall", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = TRUE)

lines(x = pd$serpos[pd$direction == 1], 
      y = pd$acc[pd$direction == 1], 
      type = "l", lty = 1)
lines(x = pd$serpos[pd$direction == 2], 
      y = pd$acc[pd$direction == 2], 
      type = "l", lty = 2)
legend(1, 1.0, c("Forward", "Backward"), lty = 1:2,
       horiz = FALSE, cex = 0.5, yjust = 1, xjust = 0)

}
\keyword{datasets}
