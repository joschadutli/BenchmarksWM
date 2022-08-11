\name{benchmarkWM9}
\alias{benchmarkWM9}
\docType{data}
\encoding{UTF-8}
\title{Working Memory Benchmark 9: Effects of Distinctiveness and Grouping}

\description{
  Subsection of the Working Memory (WM) Benchmarks datasets (Oberauer, 2019) associated
  with the effects of distinctiveness and grouping. This documentation summarizes the
  subsections of Benchmark 9 in Oberauer et al. (2018) and contains data on the effects
  of temporal distinctiveness and grouping of stimuli within lists.
  See Oberauer et al. (2018; 908f.) for a more precise description of the datasets and the
  Benchmark findings they represent.
}
\usage{data("benchmarkWM9")}
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
\strong{4 datasets on the effects of temporal distinctiveness and grouping in WM.}
\describe{
  \item{\code{\link{morin10}}}{The temporal isolation effect in recognition memory (BM 9.1.1)}
  \item{\code{\link{nimmo06}}}{Boundaries of the temporal isolation effect with unpredictable presentation schedules (BM 9.1.2)}
  \item{\code{\link{frankish89}}}{Effects of grouping of stimuli in a list (BM 9.2)}
  \item{\code{\link{farrell09}}}{Effects of grouping and interposition errors (BM 9.2; BM 9.2.3)}
  }
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{

  Farrell, S., & Lelièvre, A. (2009). End anchoring in short-term order memory. \emph{
  Journal of Memory and Language, 60}, 209-227. http://dx.doi.org/10.1016/j.jml.2008.09.004

  Frankish, C. (1989). Perceptual organization and precategorical acoustic storage.
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 15}, 469-479.
  http://dx.doi.org/10.1037/0278-7393.15.3.469

  Morin, C., Brown, G. D. A., & Lewandowsky, S. (2010). Temporal isolation effects in
  recognition and serial recall. \emph{Memory & Cognition, 38}, 849-859. http://dx.doi.org/10.3758/MC.38.7.849

  Nimmo, L. M., & Lewandowsky, S. (2006). Distinctiveness revisited: Unpredictable
  temporal isolation does not benefit short-term serial recall of heard or seen events.
  \emph{Memory & Cognition, 34}, 1368-1375. http://dx.doi.org/10.3758/BF03193278

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

}
\examples{
  data(benchmarkWM9)
  data(frankish89)
  ?farrell09
}
\keyword{datasets}
