\name{mcelree89}
\alias{mcelree89}
\docType{data}
\title{
    McElree & Dosher's (1989) set-size effect on accuracy in item recognition
}
\description{
  Group-level data for McElree & Dosher's (1989) set-size effect
  on accuracy in an item recognition task using single-word visual stimuli.
}
\usage{data("mcelree89")}
\format{
  A data frame with 18 observations on the following 3 variables.
  \describe{
    \item{\code{size}}{Number of items in the presented list:  3-6}
    \item{\code{pos}}{Serial position of probe item: 1-size, where 1 is the
                      most recent}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the group-level data reported by McElree & Dosher (1989) ,
  listed as a benchmark dataset in working memory by Oberauer et
  al. (2018, BM1.1), and archived in Oberauer (2019). They comprise the
  mean proportion correct scores across 18 psychology
  undergraduates. All manipulations were within-subject.

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  McElree, B., and Dosher, B.A. (1989). Serial position and set size in
  short-term memory: The time course of recognition. \emph{Journal of
  Experimental Psychology: General, 118}, 346-373.
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
    
}
\examples{

    ## Reproducing Figure 2C of Oberauer et al. (2018).
    data(mcelree89)
    library(tidyverse)
    elplot  <- mcelree89 \%>\% group_by(size) \%>\%
               summarise(acc=mean(acc))
    elplot \%>\% ggplot(aes(x=size, y=acc)) +
                 geom_point() + geom_line() +
                 ylim(.5,1)

}

\keyword{datasets}

