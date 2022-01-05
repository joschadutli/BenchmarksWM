\name{verhaeghen05}
\alias{verhaeghen05} 
\docType{data}
\title{
  Verhaeghen & Basak's (2005)  set-size effect on accuracy in a modified
  N-back task
}
\description{
  Group-level data for Verhaeghen & Basak's (2005) set-size effect on
  accuracy in a modified N-back task using single-digit visual stimuli.
}
\usage{data("verhaeghen05")}
\format{
  A data frame with 5 observations on the following 2 variables.
  \describe{
    \item{\code{back}}{Nature of N-back task: 1-5}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the group-level data from Verhaeghen & Basak (2005), listed
  as a benchmark dataset in working memory by Oberauer et al. (2018,
  BM1.1), and archived in Oberauer (2019) by estimation from Figure 2,
  right panel, 'younger' group, of Verhaeghen & Basak (2005). They
  comprise the mean proportion correct scores across 28 US psychology
  students. All manipulations were within-subject.

  NOTE: 30 participants were tested, 2 were excluded due to below 90\%
  accuracy at back = 1. 

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Verhaeghen, P. & Basask, C. (2005). Ageing and switching of the focus
  of attention in working memory: Results from a modified N-back
  task. \emph{Quarterly Journal of Experimental Psychology, 58A},
  134-154. 
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
    
}
\examples{

    ## Reproducing Figure 2D, 'columns' of Oberauer et al. (2018).
    data(verhaeghen05)
    library(tidyverse)
    verhaeghen05 \%>\% ggplot(aes(x=back, y=acc)) + geom_point() +
                    geom_line() + ylim(0.5, 1)

}

\keyword{datasets}

