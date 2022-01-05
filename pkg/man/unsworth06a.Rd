\name{unsworth06a}
\alias{unsworth06a}
\docType{data}
\title{
  Unsworth & Engle's (2006a) set-size effect on accuracy in serial recall
}
\description{
  Individual-subject data for Unsworth & Engle's (2006a) set-size effect
  on accuracy in serial recall with simple and complex span tests using
  verbal materials.
}
\usage{data("unsworth06a")}
\format{
  A data frame with 4935 observations on the following 4 variables.
  \describe{
    \item{\code{subject}}{Unique ID number for each subject.}
    \item{\code{type}}{Type of test: 'simple', 'complex'}
    \item{\code{test}}{Test used: 'word', 'letter', 'operation', or 'reading'}
    \item{\code{size}}{Set size: 2-8.}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the individual-subject data collected by Kane et al. (2004),
  further analysed by Unsworth & Engle (2006a), listed as a benchmark
  dataset in working memory by Oberauer et al. (2018, BM1.1), and
  archived in Oberauer (2019). The comprise the proportion correct
  scores for 235 U.S. adults aged 18-35. Each participant completed four
  serial recall working memory tests, using: (a) words, (b) letters, (c)
  words presented within a math operations task ('operation'), (d)
  letters presented within a sentence-reading task ('reading'). In
  Unsworth & Engle (2006a), tests (a) and (b) are classified as simple
  recall tasks; tests (c) and (d) as complex recall tasks.

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{ 

  Kane, M.J., Hambrick, D.Z., Tuholski, S.W., Wilhelm, O., Payne, T.W.,
  and Engle, R.W. (2004). The generality of working memory capacity: A
  latent-variable approach to verbal and visuospatial memory span and
  reasoning. \emph{Journal of Experimental Psychology: General, 133}, 189-217.

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Unsworth, N., and Engle, R.W. (2006a). Simple and complex memory spans
  and their relation to fluid abilities: Evidence from list-length
  effects. \emph{Journal of Memory and Language, 54}, 68-80.
  
}
\examples{

    ## Reproducing Figure 2A of Oberauer et al. (2018).
    data(unsworth06a)
    library(tidyverse)
    unsplot <- unsworth06a \%>\% filter(size < 8) \%>\%
               group_by(type, size) \%>\%
               summarise(acc=mean(acc))
    unsplot \%>\% ggplot(aes(x=size, y=acc, color=type)) +
                  geom_point() + geom_line() + ylim(0,1)
}

\keyword{datasets}
