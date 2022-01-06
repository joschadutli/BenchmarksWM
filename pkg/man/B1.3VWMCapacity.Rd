\name{B1.3VerbalWMCapacity}
\alias{B1.3VerbalWMCapacity}
\alias{B1.3 VerbalWMCapacity}
\alias{verbalWMCapacity}
\alias{NChunks}

\title{Working Memory Benchmarks: Verbal Working Memory Capacity Limits:\cr
\emph{Number of Items Recalled or Recognized}}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with verbal working memory capacity. The data show a core capacity limit of
  about three chunks when articulatory rehearsal (i.e. chunking) is suppressed.
  See section "Details" for more precise descriptions of the datasets.
}

\details{
  Benchmark dataset in Working Memory on the number of items recalled as 
  listed by Oberauer et al. (2018) and archived by Oberauer (2019). 
  \itemize{
  \item{Chen & Cowan's (2009) data show a core verbal working memory capacity
  limit of about three chunks with articulatory suppression.}
  }
}

\format{
  \strong{1 dataset on verbal working memory capacity limits.}
  \describe{
  \item{\code{\link{chen09}}}{Core verbal working memory capacity of about for chunks when chunking is suppressed.}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Chen, Z., & Cowan, N. (2009). Core verbal working-memory capacity: The limit in
  words retained without covert articulation. \emph{Quarterly Journal of 
  Experimental Psychology: Human Experimental Psychology, 62}, 1420-1429. 
  http://dx.doi.org/10.1080/17470210802453977
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
  data("chen09")
  ?chen09
}
