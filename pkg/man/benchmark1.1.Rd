\name{benchmark1.1}
\alias{benchmark1.1}
\docType{data}
\encoding{UTF-8}
\title{Working Memory Benchmarks: Set Size Effects on Accuracy}

\description{
  Summary dataset relating to the subsection of the Working Memory
  Benchmarks datasets (Oberauer, 2019) associated with set size effects
  on accuracy measures. Overall, the data show a monotonic
  decline of accuracy with increasing set size. See section "Details" for
  links to the documentation files providing more precise descriptions
  of the single datasets included in this Benchmark.

}
\usage{data("benchmark1.1")}
\format{
  A data frame with 7 observations on the following 6 variables.
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
\strong{7 datasets with effects of set size on accuracy.}
\describe{
  \item{\code{\link{unsworth06a}}}{Set size effect on accuracy in serial recall for simple and complex span tasks.}
    \item{\code{\link{bunting06}}}{Set size effect on accuracy in a running memory span task.}
    \item{\code{\link{mcelree89}}}{Set size effect on accuracy in item recognition with words as stimuli.}
    \item{\code{\link{jonides97}}}{Effects of increasing N in a standard N-back task on accuracy.}
    \item{\code{\link{verhaeghen05}}}{Effects of increasing N in a modified N-back task on accuracy.}
    \item{\code{\link{oberauer01}}}{Effects of set size on accuracy in a working memory updating task.}
    \item{\code{\link{adam15}}}{Effects of set size on accuracy in change detection with colored squares as stimulus material.}
}
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Adam, K.C.S., Mance, I., Fukuda, K., & Vogel, E.K. (2015). The
  contribution of attentional lapses to individual differences in visual
  working memory capacity. \emph{Journal of Cognitive Neuroscience, 27},
  1601-1616.

  Bunting, M., Cowan, N. & Saults, J.S. (2006). How does running
  memory span work? \emph{Quarterly Journal of Experimental Psychology,
  59}, 1691-1700.

  Jonides, J., Schumacher, E.H., Smith, E.E., Lauber, E.J., Awh, E.,
  Minoshima, S., & Koeppe, R.A. (1997). Verbal working memory load
  affects regional brain activation as measured by PET. \emph{Journal of
  Cognitive Neuroscience, 94}, 462-475.

  McElree, B., & Dosher, B.A. (1989). Serial position and set size in
  short-term memory: The time course of recognition. \emph{Journal of
  Experimental Psychology: General, 118}, 346-373.

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., & Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Oberauer, K., & Kliegl, R. (2001). Beyond resources: Formal models of
  complexity effects and age differences in working memory. \emph{The
  European Journal of Cognitive Psychology, 13}, 187-215.

  Unsworth, N., & Engle, R.W. (2006a). Simple and complex memory spans
  and their relation to fluid abilities: Evidence from list-length
  effects. \emph{Journal of Memory and Language, 54}, 68-80.

  Verhaeghen, P. & Basask, C. (2005). Ageing and switching of the focus
  of attention in working memory: Results from a modified N-back
  task. \emph{Quarterly Journal of Experimental Psychology, 58A},
  134-154.

}
\examples{
  data(benchmark1.1)
  data("unswort06a")
  ?unswort06a
}
\keyword{datasets}
