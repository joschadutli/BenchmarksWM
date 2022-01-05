\name{Benchmarks-Overview}
\alias{Overview}
\alias{BenchmarksWM-Overview}

\title{An Overview of the Working Memory Research Benchmark Dataset Categories as in Oberauer et al. (2018)}

\description{
  An overview of the subsections of Benchmarks in Working Memory research
  as listed in Oberauer et al. (2018). The subsections include a description
  of the categories of findings as well as information about the respective
  datasets that were archived by Oberauer (2019).
}
\format{
  A list of categories of Benchmark data in Working Memory research from Oberauer et al. (2018).
  \describe{
  \item{\code{\link{B1.1 SetsizeAccuracy}}}{Set size effects on accuracy.}
  \item{\code{\link{B1.2 SetsizeRT}}}{Set size effects on response times.}
  \item{\code{\link{B1.3 VerbalWMCapacity}}}{Capacity limits in verbal Working Memory.}
  \item{\code{\link{B2.1 RetentionInterval}}}{Effects of filled retention intervals.}
  \item{\code{\link{B2.4 PresentationDuration}}}{Effects of presentation duration of memory items.}
  \item{\code{\link{B3 SerialPositionEffects}}}{Effects of serial position.}
  \item{\code{\link{B4 ErrorCharacteristics}}}{Characteristics of errors.}
  \item{\code{\link{B5 MultipleDemands}}}{Effects of multiple tasks and multiple stimulus domains.}
  \item{\code{\link{B6 IrrelevantSounds}}}{Effects of irrelevant sounds during WM tasks.}
  \item{\code{\link{B7 WordLengthEffect}}}{Effects of syllable-based word length.}
  \item{\code{\link{B8 SimilarityEffects}}}{Effects of phonological similarity and of item-probe similarity in change detection tasks.}
  \item{\code{\link{B9 DistinctivenessGrouping}}}{Effects of temporal distinctiveness and of grouping of lists.}
  \item{\code{\link{B10 PrioritizationOfInformation}}}{Effects of prioritization in WM (e.g. retro-cues)}
  \item{\code{\link{B11 EffectsOfKnowledge}}}{Effects of knowledge such as chunking or the Hebb repitition effect.}
  \item{\code{\link{B12 IndividualDifferences}}}{Individual differences in WM performance and capacity.}
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
?B1.2SetsizeRT
data(unsworth06a)
}