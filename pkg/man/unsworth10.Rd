\name{unsworth10}
\alias{unsworth10}
\docType{data}
\title{
  Unsworth, Spillers, & Brewer's (2010) Data on Primary and Secondary Memory
}
\description{
  These are the individual-subject data from Unsworth, Spillers and Brewer (2010).
  In immediate free recall of lists of ten words, the authors were able to distinguish 
  between recall from primary memory (PM) and secondary memory (SM) in a factor analysis. 
  PM is defined as the memory component that actively maintains information over the 
  short-term. SM, on the other hand, retrieves information that could not be actively
  maintained in PM due to a large number of memory items. In their data, PM and SM account
  for unique variance in Working Memory Capacity (WMC). Moreover, the WMC factor in a 
  structural equation model is correlated with a factor of general fluid intelligence.
}
\usage{data("unsworth10")}
\format{
  A data frame with 140 observations on the following 23 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant.}
    \item{\code{operation.span}}{Operaction span score}
    \item{\code{symmetry.span}}{Symmetry span score}
    \item{\code{reading.span}}{Reading span score}
    \item{\code{number.series}}{Score in number series task}
    \item{\code{verbal.analogies}}{Score in verbal analogies task}
    \item{\code{verbal.SAT}}{Verbal SAT score}
    \item{\code{quantitative.SAT}}{Quantitative SAT score}
    \item{\code{free.recall}}{Immediate free recall score (1-10)}
    \item{\code{prim.mem.1}}{PM score (from the first half of the experimental lists)}
    \item{\code{prim.mem.2}}{PM score (from the second half of the experimental lists)}
    \item{\code{second.mem.1}}{SM score (from the first half of the experimental lists)}
    \item{\code{second.mem.2}}{SM score (from the second half of the experimental lists)}
    \item{\code{serpos1} - \code{serpos10}}{Mean score at each serial position}
  }
}
\details{
  The authors report data from 135 participants instead of 140 included in the data set. 
  The 5 excluded participants are presumably numbers 45, 97, 101, 103, and 139 as the 
  immediate free recall measures are missing from these five datasets.
  For further details (e.g. how the PM and SM component measures were obtained and how the
  different span and general abilities tasks were structured), see Unsworth, Spillers, &
  Brewer (2010).
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
  
  Unsworth, N., Spillers, G. J., & Brewer, G. A. (2010). The contributions of primary 
  and secondary memory to working memory capacity: An individual differences analysis 
  of immediate free recall. \emph{Journal of Experimental Psychology: Learning, Memory, 
  and Cognition, 36}, 240-247. http://dx.doi.org/10.1037/a0017739
  
}
\examples{
data(unsworth10)
}
\keyword{datasets}
