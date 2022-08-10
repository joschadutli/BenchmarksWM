\name{benchmarks.overview}
\alias{benchmarks.overview}
\docType{data}
\encoding{UTF-8}
\title{An Overview of the Working Memory Research Benchmark Dataset Categories as in Oberauer et al. (2018)}

\description{
  An overview of the subsections of Benchmarks in Working Memory research
  as listed in Oberauer et al. (2018). The subsections include a description
  of the categories of findings as well as information about the respective
  datasets that were archived by Oberauer (2019).
}
\usage{data("benchmarks.overview")}
\format{
  A data frame with 15 observations on the following 3 variables.
  \describe{
    \item{\code{name}}{benchmark number}
    \item{\samp{benchmark finding}}{finding associated with the benchmark}
    \item{\samp{number of datasets}}{number of associated datasets available in the package}
  }
}
\details{
  \strong{A list of categories of Benchmark data in Working Memory research from Oberauer
  et al. (2018).}
  \describe{
  \item{\code{\link{benchmark1.1}}}{Set size effects on accuracy.}
  \item{\code{\link{benchmark1.2}}}{Set size effects on response times.}
  \item{\code{\link{benchmark1.3}}}{Capacity limits in verbal Working Memory.}
  \item{\code{\link{benchmark2.1}}}{Effects of filled retention intervals.}
  \item{\code{\link{benchmark2.4}}}{Effects of presentation duration of memory items.}
  \item{\code{\link{benchmark3}}}{Effects of serial position.}
  \item{\code{\link{benchmark4}}}{Characteristics of errors.}
  \item{\code{\link{benchmark5}}}{Effects of multiple tasks and multiple stimulus domains.}
  \item{\code{\link{benchmark6}}}{Effects of irrelevant sounds during WM tasks.}
  \item{\code{\link{benchmark7}}}{Effects of syllable-based word length.}
  \item{\code{\link{benchmark8}}}{Effects of phonological similarity and of item-probe similarity in change detection tasks.}
  \item{\code{\link{benchmark9}}}{Effects of temporal distinctiveness and of grouping of lists.}
  \item{\code{\link{benchmark10}}}{Effects of prioritization in WM (e.g. retro-cues)}
  \item{\code{\link{benchmark11}}}{Effects of knowledge such as chunking or the Hebb repitition effect.}
  \item{\code{\link{benchmark12}}}{Individual differences in WM performance and capacity.}
  }
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
}
\examples{
  data(benchmarks.overview)
  help("benchmarks.overview")
  ?benchmark1.1
  help("benchmark3")
  data(unsworth06a)
}
\keyword{datasets}
