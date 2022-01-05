\name{kane04}
\alias{kane04}
\docType{data}
\title{
  Data from Kane et al. (2004) on the Generality of Working Memory Capacity with Respect to
  Individual Differences 
}
\description{
  These are the individual-subject data on a battery of short-term memory (STM), working
  memory (WM) and reasoning tasks from Kane et al. (2004). The data show that individual
  differences in WM capacity (i.e. task performance) is a domain-general predictor for
  fluid intelligence whereas STM performance is more of a domain-specific predictor with 
  respect to reasoning abilities.
}
\usage{data("kane04")}
\format{
  A data frame with 235 observations on the following 26 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \strong{Proportion correct in STM tasks}
    \itemize{
    \item{\code{word.span}}{ (verbal STM)}
    \item{\code{letter.span}}{ (verbal STM)}
    \item{\code{digit.span}}{ (verbal STM)}
    \item{\code{ball.span}}{ (spatial STM)}
    \item{\code{arrow.span}}{ (spatial STM)}
    \item{\code{matrix.span}}{ (spatial STM)}
    }
    \strong{Proportion correct in WM tasks}
    \itemize{
    \item{\code{operation.span}}{ (verbal WM)}
    \item{\code{counting.span}}{ (verbal WM)}
    \item{\code{reading.span}}{ (verbal WM)}
    \item{\code{navigation.span}}{ (spatial WM)}
    \item{\code{symmetry.span}}{ (spatial WM)}
    \item{\code{rotation.span}}{ (spatial WM)}
    }
    \strong{Proportion correct in reasoning tasks}
    \itemize{
    \item{\code{ets.inference}}{ ETS inference test (verbal reasoning)}
    \item{\code{paper.folding}}{ ETS Paper Folding Test (spatial visualization)}
    \item{\code{afq.analogies}}{ AFOQT Analogies Test (verbal reasoning)}
    \item{\code{dat.space.relations}}{ DAT Space Relations Test (spatial visualization)}
    \item{\code{remote.associates}}{ Remote Associates Test (verbal reasoning)}
    \item{\code{afq.rotated.blocks}}{ AFOQT Rotated Blocks Test (spatial visualization)}
    \item{\code{ets.nonsense.syllogisms}}{ ETS Nonsense Syllogisms Test (verbal reasoning)}
    \item{\code{ets.surface.development}}{ ETS Surface Development Test (spatial visualization)}
    \item{\code{afq.reading.comprehension}}{ AFOQT Reading Comprehension (verbal reasoning)}
    \item{\code{ets.form.board}}{ ETS Form Board Test (spatial visualization)}
    \item{\code{wasi.matrices}}{ WASI Matrix Reasoning (figural inductive reasoning–Gf)}
    \item{\code{raven.matrices}}{ RAPM, Set II (figural inductive reasoning–Gf)}
    \item{\code{beta3.matrices}}{ BETA III Matrix Reasoning (figural inductive reasoning–Gf)}
    }
  }
}
\details{
  See \code{\link{kane04t}} for the trial-level data in STM and WM tasks. 
  For further details, see Kane et al. (2004).
}
\source{
     Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Kane, M. J., Hambrick, D. Z., Tuholski, S. W., Wilhelm, O., Payne, T. W., & Engle, R. W.
  (2004). The generality of working memory capacity: A latent-variable approach to verbal 
  and visuospatial memory span and reasoning. \emph{Journal of Experimental Psychology:
  General, 133}, 189– 217. http://dx.doi.org/10.1037/0096-3445.133.2.189
  
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(kane04)
## maybe str(kane04) ; plot(kane04) ...
}
\keyword{datasets}
