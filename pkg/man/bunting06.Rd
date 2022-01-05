\name{bunting06}
\alias{bunting06}
\docType{data}
\title{
    Bunting et al.'s (2006) set-size effect on accuracy in running memory span
}
\description{
  Individual-subject data for Bunting et al.'s (2006) set-size effect
  on accuracy in a running memory span task using single-digit auditory
  stimuli. 
}
\usage{data("bunting06")}
\format{
  A data frame with 1400 observations on the following 6 variables.
  \describe{
    \item{\code{subject}}{Unique ID number for each subject.}
    \item{\code{gender}}{Gender of participant: "female", "male"}
    \item{\code{speed}}{Rate of presentation of digits: "fast" or "slow"}
    \item{\code{span}}{Number of items participant was asked to recall:  1-7}
    \item{\code{pos}}{Serial position of item: 1-span, where 1 is the
                      most recent}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the individual-subject data collected by Bunting et
  al. (2006), listed as a benchmark dataset in working memory by
  Oberauer et al. (2018, BM1.1), and archived in Oberauer (2019). They
  comprise the proportion correct scores for 25 U.S. right-handed
  undergraduates. Stimuli were digits presented either at a rate of four
  per second ("fast") or one per second ("slow"). All manipulations were
  within-subject.

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Bunting, M., Cowan, N. and Saults, J.S. (2006). How does running
  memory span work? \emph{Quarterly Journal of Experimental Psychology,
  59}, 1691-1700.

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
    
}
\examples{

    ## Reproducing Figure 2B of Oberauer et al. (2018).
    data(bunting06)
    library(tidyverse)
    bunplot <- bunting06 \%>\% group_by(speed, span) \%>\%
               summarise(acc=mean(acc))
    bunplot \%>\% ggplot(aes(x=span, y=acc, color=factor(speed))) +
                  geom_point() + geom_line() + ylim(0,1)
}

\keyword{datasets}

