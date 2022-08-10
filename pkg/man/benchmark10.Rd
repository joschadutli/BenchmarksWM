\name{benchmark10}
\alias{benchmark10}
\docType{data}
\encoding{UTF-8}
\title{Working Memory Benchmark 10: Prioritization of Information in WM}

\description{
  Subsection of the Working Memory (WM) Benchmarks datasets (Oberauer, 2019) associated
  with the effects of retro cues to individual items and item-switches. This documentation
  summarizes the two subsections of Benchmark 10 on prioritization of information in WM
  in Oberauer et al. (2018).
  See Oberauer et al. (2018; 910f.) for a more precise description of the datasets and the
  Benchmark findings they represent.
}
\usage{data("benchmark10")}
\format{
  A data frame with 4 observations on the following 5 variables.
  \describe{
    \item{\code{dataset}}{Names of the datsets}
    \item{\code{manipulation}}{Which variable(s) were manipulated}
    \item{\code{response.variable}}{Outcome variable(s)}
    \item{\code{memory.test}}{Type of memory test}
    \item{\code{stimulus.material}}{Type of stimulus materials}
  }
}
\details{
\strong{2 datasets on the effects of retro cues to individual items in WM.}
  \describe{
  \item{\code{\link{shepherdson18}}}{Retro-cue effect in a change detection task in visual WM
  (\code{Exp S1a} and \code{Exp S1b}) described in Souza, Rerko, & Oberauer (2014) (BM 10.1) and
  in verbal WM (\code{Exp 1} and \code{Exp 2}) described in Shepherdson, Oberauer, & Souza (2018).}
  \item{\code{\link{oberauer17}}}{Retro-cue effect in a continuous reproduction task in visual WM (BM 10.1).}
  }
  \strong{2 datasets on item-switch effects in WM.}
  \describe{
  \item{\code{\link{hedge13}}}{Switch costs in an updating task in visual WM. (BM 10.2)}
  \item{\code{\link{oberauer06}}}{Item-switch effects and the change of their degree with practice in an n-back task with digits as stimulus material. (BM 10.2)}
  }
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{

  Hedge, C., & Leonards, U. (2013). Using eye movements to explore switch costs in working
  memory. \emph{Journal of Vision, 13}(4):18, 1-19. http://dx.doi.org/10.1167/13.4.18

  Oberauer, K. (2006). Is the focus of attention in working memory expanded through
  practice? \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 32},
  197-214. http://dx.doi.org/10.1037/0278-7393.32.2.197

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Oberauer, K., & Lin, H. Y. (2017). An interference model of visual working memory.
  \emph{Psychological Review, 124}(1), 21. http://dx.doi.org/10.1037/rev0000044

  Souza, A. S., Rerko, L., & Oberauer, K. (2014). Unloading and reloading working memory:
  Attending to one item frees capacity. \emph{Journal of Experimental Psychology: Human
  Perception and Performance, 40}, 1237-1256. http://dx.doi.org/10.1037/a0036331

}
\examples{
  data(benchmark10)
  data(oberauer06)
  ?souza14
}
\keyword{datasets}
