\name{farrell13}
\alias{farrell13}
\docType{data}
\title{
  Data on the Fill-In Effect of Errors in Forward Serial Recall (Farrell, Hurlstone, &
  Lewandowsky, 2013)
}
\description{
  Individual-trial data from 19 single experiments and 21 different datasets analyzed for 
  output position errors in forward serial recall by Farrell, Hurslstone, and Lewandowsky 
  (2013). The data show the so-called fill-in effect: An output position error of starting 
  with B instead of A in a sequence A-B-C results in an output order B-A-C and not in B-C-A.
  Thus, position errors are more likely to result in switching positions rather than skipping
  positions.
}
\usage{data("farrell13")}
\format{
  A data frame with 237433 observations on the following 6 variables.
  \describe{
    \item{\code{exp_code}}{Experiment label as in Table 1 in Farrell et al. (2013)}
    \item{\code{exp_name}}{Name of the dataset}
    \item{\code{id}}{Unique subject ID for each participant within each experiment}
    \item{\code{serpos}}{Serial input position}
    \item{\code{outpos}}{Output position: \itemize{\item{1-Set size: output position}
    \item{\code{-1}: omission}
    \item{\code{-9}: extra-list intrusion}
    \item{\code{NA}: no data for this serial position in this experiment.}}}
    \item{\code{correct}}{correct/incorrect: 1/0 (regardless of output position)}
  }
}
\details{
  For details, see Farrell et al. (2013).
}
\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Farrell, S., Hurlstone, M. J., & Lewandowsky, S. (2013). Sequential dependencies in recall
  of sequences: Filling in the blanks. \emph{Memory & Cognition, 41}, 938–952. 
  http://dx.doi.org/10.3758/s13421-013-0310-0
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
data(farrell13)

}
\keyword{datasets}
