\name{benchmarkWM2.4}
\alias{benchmarkWM2.4}
\encoding{UTF-8}
\docType{data}

\title{Working Memory Benchmarks: Effects of Presentation Duration}

\description{
  Summary dataset relating to the subsection of the Working Memory
  Benchmarks datasets (Oberauer, 2019) associated
  with the effects of presentation duration. Overall, longer
  presentation times increase the probability and/or precision
  of recall. See section "Details" for
  links to the documentation files providing more precise descriptions
  of the single datasets included in this Benchmark.
}
\details{
  \strong{4 datasets with effects of presentation duration.}
  \describe{
    \item{\code{\link{tan08}}}{Effects of presentation duration on memory performance
      in verbal WM.}
    \item{\code{\link{bays11}}}{Effects of presentation duration on memory performance
      in visual WM.}
    \item{\code{\link{grenfell13}}}{Effects of presentation duration on memory performance
      in verbal WM with and without articulatory suppression.}
    \item{\code{\link{ricker17}}}{Effects of presentation duration on memory performance
      in visual WM controlling for serial position.}
  }
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{

  Bays, P. M., Gorgoraptis, N., Wee, N., Marshall, L., & Husain, M. (2011).
  Temporal dynamics of encoding, storage, and reallocation of visual working
  memory. \emph{Journal of Vision, 11}, 1-15. http://dx.doi.org/10.1167/11.10.6

  Grenfell-Essam, R., Ward, G., Tan, L. (2013). The Role of Rehearsal on the
  Output Order of Immediate Free Recall of Short and Long Lists.
  \emph{Journal of Experimental Psychology: Learning, Memory, and
    Cognition, 39}, 317-347. http://dx.doi.org/10.1037/a0028974

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Ricker, T. J., Hardman, K. O. (2017). The Nature of Short-Term Consolidation
  in Visual Working Memory. \emph{Journal of Experimental Psychology:
      General, 146, 11}, 1551-1573. http://dx.doi.org/10.1037/xge0000346

  Tan, L., & Ward, G. (2008). Rehearsal in immediate serial recall.
  \emph{Psychonomic Bulletin & Review, 15}, 535-542.
  http://dx.doi.org/10.3758/PBR.15.3.535
}
\examples{
  data(ricker17)
  ?bays11
}
\keyword{datasets}
