\name{benchmark1.3}
\alias{benchmark1.3}
\encoding{UTF-8}
\docType{data}

\title{Working Memory Benchmarks: Verbal Working Memory Capacity Limits:\cr
\emph{Number of Items Recalled or Recognized}}

\description{
  Summary dataset relating to the subsection of the Working Memory
  Benchmarks datasets (Oberauer, 2019) associated
  with verbal working memory capacity. The data show a core capacity limit of
  about three chunks when articulatory rehearsal (i.e. chunking) is suppressed.
  See section "Details" for links to the documentation files
  providing more precise descriptions
  of the single datasets included in this Benchmark.
}
\usage{data("benchmark1.3")}
\format{
  A data frame with 1 observations on the following 6 variables.
  \describe{
    \item{\code{dataset}}{Names of the datsets}
    \item{\code{manipulation}}{Which variable(s) were manipulated}
    \item{\code{response.variable}}{Outcome variable(s)}
    \item{\code{task}}{Task used in the study}
    \item{\code{memory.test}}{Type of memory test}
    \item{\code{stimulus.material}}{Type of stimulus materials}
  }
}
\details{
\strong{1 dataset on verbal working memory capacity limits.}
\describe{
  \item{\code{\link{chen09}}}{Core verbal working memory capacity of about for chunks when chunking is suppressed.}
  }
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Chen, Z., & Cowan, N. (2009). Core verbal working-memory capacity: The
  limit in words retained without covert articulation. \emph{Quarterly
  Journal of Experimental Psychology: Human Experimental Psychology,
  62}, 1420-1429. http://dx.doi.org/10.1080/17470210802453977

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  }
\examples{
data(benchmark1.3)
data("chen09")
?chen09
}
\keyword{datasets}
