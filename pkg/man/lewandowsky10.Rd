\name{lewandowsky10}
\alias{lewandowsky10}
\docType{data}
\title{
  Lewandowsky, Geiger, Morrell, & Oberauer's (2010) data on Forgetting in Verbal
  Complex Span Tasks
}
\description{
  Individual-trial data from Experiment 2 in Lewandowsky et al. (2010) on the
  effect of filled time intervals in a verbal complex span task. Participants either
  had an unfilled time interval between list items, were presented with 1 
  distractor, 3 identical distractors or 3 different distractors. Forgetting only
  increases for longer filled time intervals when the distractor task involves
  changing materials.
}
\usage{data("lewandowsky10")}
\format{
  A data frame with 1440 observations on the following 5 variables.
  \describe{
    \item{\code{subject}}{Unique subject ID for each participant}
    \item{\code{trial}}{Unique trial ID within each participant}
    \item{\code{cond}}{Conditions: \itemize{\item{\code{0} = Quiet}
    \item{\code{1} = 1 distractor} \item{\code{2} = 3 identical distractors}
    \item{\code{3} = 3 different distractors}}}
    \item{\code{rt}}{Response time (s)}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{
  These are the individual-trial data  collected and analyzed by Lewandowsky et
  al. (2010) in Experiment 2, listed as a benchmark dataset by Oberauer et al. 
  (2018, BM2.1) and archived by Oberauer (2019). They contain proportion correct 
  and response time scores for different conditions in a verbal complex span task.
  Increasing filled time intervals between to-be-remembered list items only results
  in more forgetting (i.e. recall of fewer items) when the distractor task consists
  of changing materials. 
}
\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Lewandowsky, S., Geiger, S. M., Morrell, D. B., & Oberauer, K. (2010). Turning
  simple span into complex span: Time for decay or interference from distractors?
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 36},
  958–978. http://dx.doi.org/10.1037/a0019764
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
data(lewandowsky10)
### reproduce Figure 5 (Oberauer et al., 2018)

plotd <- aggregate(acc ~ cond, data = lewandowsky10, FUN = mean)
plot(x=c(0,3), y=c(0.3,1.0), type = "n", xlab = "Condition", 
     ylab = "Proportion Correct", main = "Forgetting in Verbal Complex Span",
     xaxt = "n")
axis(side = 1, at = c(0,1,2,3), labels = c("Quiet", "1 distractor", "3 identical distr.", "3 different distr."))
lines(x = plotd$cond, y = plotd$acc, type = "b", lty = 1, pch = 15)
}
\keyword{datasets}
