\name{floden10}
\alias{floden10}
\docType{data}
\title{
  Floden, Stuus, & Craik's (2010) Data on Age Differences in the Brown-Peterson Task
}
\description{
  Individual-subject data from Experiment 2 in Floden, Stuus, Craik (2010) on age
  differences in the Brown-Peterson task. Participants were presented with lists
  of four words followed by a filled retention interval with varied lengths. During
  the retention interval, participants had to perform a subtraction task on 
  presented digits. Older adults recalled fewer words and overall, recall decreased
  with increasing length of filled retention interval.
}
\usage{data("floden10")}
\format{
  A data frame with 240 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{group}}{Age Group: \code{"old"} vs. \code{"young"}}
    \item{\code{age}}{Age}
    \item{\code{RI}}{Retention Interval in seconds: \code{0}, \code{3}, \code{18}, \code{36}, \code{60}}
    \item{\code{acc}}{Proportion of correct answers for each level of RI}
  }
}
\details{
  These are the individual-subject data collected and analyzed by Floden, Stuus,
  & Craik (2010), listed as a benchmark dataset in Oberauer et al. (2018, BM2.1)
  and archived by Oberauer (2019). They contain the proportion correct scores 
  for different lengths of filled retention intervals for a group of younger and
  a group of older adults. The stimulus material were words. Overall, younger 
  adults recall more words than older adults and the proportion of correctly
  recalled words decreases with increasing length of retention interval.
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  Floden, D., Stuss, D. T., & Craik, F. I. M. (2000). Age differences in 
  performance on two versions of the Brown-Peterson task. \emph{Aging, 
  Neuropsychology and Cognition, 7}, 245-259. http://dx.doi.org/10.1076/anec
  .7.4.245.795
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(floden10)
### Reproduce Figure 3 from Floden, Stuus, & Craik (2010)
plotd <- aggregate(acc ~ RI*group, data = floden10, FUN = mean)

plot(c(0,60), c(0,1), type = "n", xlab = "Retention Interval (s)",
     ylab = "Proportion Correct", main = "Age Differences in the Brown-Peterson 
     Task", xaxt = "n")
axis(side = 1, at = c(0,3,18,36,60), labels = TRUE)
young <- subset(plotd, plotd$group == "young")
lines(x = young$RI, y = young$acc, type = "b", lty = 1, pch = 15)
old <- subset(plotd, plotd$group == "old")
lines(x = old$RI, y = old$acc, type = "b", lty = 2, pch = 16)

legend(1, 0.1, c("younger adults", "older adults"), lty = 1:2, pch=15:16, yjust=0)

}
\keyword{datasets}
