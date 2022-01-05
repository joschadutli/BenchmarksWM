\name{jonides97}
\alias{jonides97} 
\docType{data}
\title{
    Jonides et al.'s (1997) set-size effect on accuracy in an N-back task
}
\description{
  
  Group-level data for Jonides et al.'s (1997) set-size effect on
  accuracy in an N-back task using single-letter visual stimuli.

}
\usage{data("jonides97")}
\format{
  A data frame with 4 observations on the following 2 variables.
  \describe{
    \item{\code{back}}{Nature of N-back task: 0-3}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the group-level data from Jonides et al. (1997), listed as a
  benchmark dataset in working memory by Oberauer et al. (2018, BM1.1),
  and archived in Oberauer (2019) by estimation from Figure 2 of Jonides
  et al. (1997). They comprise the mean proportion correct scores across
  18 US female adults. All manipulations were within-subject.

  NOTE: This dataset likely over-estimates the mean level of performance
  in this task -- Jonides et al. (1997) tested 36 participants in a
  prescreen phase, and selected the 19 that scored at least 64\% on both
  matches and mismatches. One participant was subsequently dropped due
  to technical issues.
  

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Jonides, J., Schumacher, E.H., Smith, E.E., Lauber, E.J., Awh, E.,
  Minoshima, S., and Koeppe, R.A. (1997). Verbal working memory load
  affects regional brain activation as measured by PET. \emph{Journal of
  Cognitive Neuroscience, 94}, 462-475.
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
    
}
\examples{

    ## Reproducing Figure 2D, 'standard' of Oberauer et al. (2018).
    data(jonides97)
    library(tidyverse)
    jonides97 \%>\% ggplot(aes(x=back, y=acc)) + geom_point() +
                    geom_line() + ylim(0.5, 1)

}

\keyword{datasets}

