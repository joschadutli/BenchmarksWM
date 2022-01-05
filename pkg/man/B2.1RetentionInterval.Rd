\name{B2.1RetentionInterval}
\alias{B2.1RetentionInterval}
\alias{B2.1 RetentionInterval}
\alias{RetentionInterval}

\title{Working Memory Benchmarks: Effects of Filled Retention Intervals}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with the effects of filled retention or filled inter-stimulus intervals. Overall,
  the data show a monotonic decline in proportion correct scores for longer filled 
  intervals. See section "Details" for more precise descriptions of the datasets.
}

\details{
  Benchmark datasets in Working Memory on the Effects of Filled Retention Intervals
  as listed by Oberauer et al. (2018) and archived by Oberauer (2019). 
  \itemize{
    \item{Floden, Stuus, & Craik's (2010) data show a decline in correctly
    recalled words in a Brown-Peterson task when the filled retention intervals 
    increase in length.}
    \item{Lewandowsky, Geiger, Morrell, & Oberauer's (2010) data show that in a 
    verbal complex span task, longer filled intervals only result in poorer recall
    scores when the distractor task uses changing materials.}
    \item{Ricker, Spiegel, & Cowan (2014) report data on unfilled retention intervals.}
  }
}

\format{
  \strong{3 datasets with effects of filled retention intervals.}
  \describe{
  \item{\code{\link{floden10}}}{Effects of longer filled retention intervals in 
  the Brown-Peterson task with words as stimulus material.}
  \item{\code{\link{lewandowsky10}}}{Effects of longer inter-stimulus intervals
  and changing vs. identical distractors in a verbal complex span task.}
  \item{\code{\link{ricker14}}}{Effects and their boundary conditions of unfilled
  retention intervals in Working Memory.}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}

\references{
  
  Floden, D., Stuss, D. T., & Craik, F. I. M. (2000). Age differences in 
  performance on two versions of the Brown-Peterson task. \emph{Aging, 
  Neuropsychology and Cognition, 7}, 245–259. http://dx.doi.org/10.1076/anec
  .7.4.245.795
  
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
  ?floden10
}