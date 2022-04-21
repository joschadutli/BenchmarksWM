\name{rerko14}
\alias{rerko14}
\docType{data}
\title{
  Rerko, Oberauer, & Lin's (2014; Experiment 2B) Data on the Locality Constraint of
  Spatial Distance for Errors in visual Working Memory.
}
\description{
  These are the individual-trial data from Rerko et al. (2014), listed as a Benchmark in
  Oberauer et al. (2018). The data show that the probability of selecting
  a non-target color in a visual WM task decreases with the spatial euclidean distance
  between target and non-target items in an array.
}
\usage{data("rerko14")}
\format{
  A data frame with 14400 observations on the following 10 variables.
  \describe{
    \item{\code{id}}{Unique subject ID for each participant.}
    \item{\code{session}}{Experimental Session: 1,2 or 3}
    \item{\code{trial}}{Trial ID}
    \item{\code{dist1}}{Euclidean distances between 1st non-target and the target
      item. Is equal to zero for the target items.}
    \item{\code{dist2}}{Euclidean distances between 2nd non-target and the target
      item. Is equal to zero for the target items.}
    \item{\code{dist3}}{Euclidean distances between 3rd non-target and the target
      item. Is equal to zero for the target items.}
    \item{\code{dist4}}{Euclidean distances between 4th non-target and the target
      item. Is equal to zero for the target items.}
    \item{\code{dist5}}{Euclidean distances between 5th non-target and the target
      item. Is equal to zero for the target items.}
    \item{\code{dist}}{Euclidean distance between selected item and target item. NA if
      selected item not part of memory array.}
    \item{\code{correct}}{Correct: 1/0: correct/incorrect}
  }
}
\details{
  For details, see Rerko, Oberauer, & Lin (2014; Experiment 2B).
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


  Rerko, L., Oberauer, K., & Lin, H.-Y. (2014). Spatially imprecise representations in
  working memory. \emph{Quarterly Journal of Experimental Psychology: Human Experimental
  Psychology, 67}, 3-15. http://dx.doi.org/10.1080/17470218.2013.789543

}
\examples{
data(rerko14)
## Histogram reconstruction of Figure 10B in Oberauer et al. (2018)
## For a more detailed representation of the bins, see Fig. 10B in the paper
pd <- rerko14[which(rerko14$correct == 0),]
hist(pd$dist, breaks=10, xlab = "Euclidean distance",
     main = "Prob. for Spatial Distances of Errors", freq=FALSE)
}
\keyword{datasets}
