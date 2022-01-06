\name{B6IrrelevantSounds}
\alias{B6IrrelevantSounds}
\alias{B6 IrrelevantSounds}
\alias{IrrelevantSounds}

\title{Working Memory Benchmark 6: Effects of Irrelevant Sounds}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with the effects of irrelevant sounds. This documentation summarizes the 
  subsections of Benchmark 6 in Oberauer et al. (2018) and contains data on the 
  effects of irrelevant sounds as well as on the auditory deviant and the changing-state
  effects.
  See Oberauer et al. (2018; 906) for a more precise description of the 
  datasets and the Benchmark findings they represent.
}

\format{
  \strong{2 datasets on the distracting effects of irrelevant sounds in WM tasks.}
  \describe{
  \item{\code{\link{schlittmeier12}}}{The effects of speech and music as irrelevant sounds in working memory tasks.}
  \item{\code{\link{bell19}}}{The changing-state effect and the auditory deviant effect.}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Bell, R., Mieth, L., Röer, J. P., Troche, S., & Buchner, A. (2019). Preregistered 
  replication of the auditory deviant effect: A robust benchmark finding. \emph{Journal of 
  Cognition, 2}(1), 13. https://doi.org/10.5334/joc.64
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Schlittmeier, S. J., Weißgerber, T., Kerber, S., Fastl, H., & Hellbrück, J. (2012).
  Algorithmic modeling of the irrelevant sound effect (ISE) by the hearing sensation 
  fluctuation strength. \emph{Attention, Perception & Psychophysics, 74}, 194-203. 
  http://dx.doi.org/10.3758/s13414-011-0230-7
  
}

\examples{
  data(schlittmeier12)
  ?bell19
}

