\name{B11EffectsOfKnowledge}
\alias{B11EffectsOfKnowledge}
\alias{B11 EffectsOfKnowledge}
\alias{EffectsOfKnowledge}

\title{Working Memory Benchmark 11: Effects of Knowledge on WM performance}


\description{
  Subsection of the Working Memory (WM) Benchmarks datasets (Oberauer, 2019) associated
  with the effects of knowledge on WM performance. This documentation summarizes the 
  subsections on the effects of chunking, the effects word frequency and lexicality, and
  the Hebb repitition effect. 
  See Oberauer et al. (2018; 911f.) for a more precise description of the Benchmarks.
}

\format{
  \strong{5 datasets on the effects of chunking in WM. The Thalmann et al. (2019) datasets 
  are not mentioned in section BM 11 in Oberauer et al. (2018).}
  \describe{
  \item{\code{\link{portrat16}}}{Effects of chunking in a complex span task (BM 11.1)}
  \item{\code{thalmann19}}{Effects of chunking and the relationship to unchunked items in presence of chunked items (in line with BM 11.1)}
  \itemize{
  \item{\code{\link{thalmann19a}}}{: Experiment 1: Effects of pre-learned word-pairs (i.e. chunks)}
  \item{\code{\link{thalmann19b}}}{: Experiment 2: Effects of chunked letters and singletons}
  \item{\code{\link{thalmann19c}}}{: Experiment 3: Effects of chunked letters and singletons under articulatory suppression}
  \item{\code{\link{thalmann19d}}}{: Experiment 4: Effects of chunks in different list positions}
  }
  }
  \strong{1 dataset on the effects of word frequency in WM not mentioned in Oberauer et 
  al. (2018).}
  \describe{
  \item{\code{\link{quinlan17}}}{Difference between low and high-frequency words on WM performance (in line with BM 11.3).}
  }
  \strong{1 dataset on the Hebb repitition effect.}
  \describe{
  \item{\code{\link{page06}}}{The Hebb repitition effect with and without concurrent articulation (i.e. articulatory suppression; BM 11.5).}
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
  
  Page, M. P. A., Cumming, N., Norris, D., Hitch, G. J., & McNeil, A. M. (2006). Repetition
  learning in the immediate serial recall of visual and auditory materials. \emph{Journal 
  of Experimental Psychology: Learning, Memory, and Cognition, 32}, 716-733. 
  http://dx.doi.org/10.1037/0278-7393.32.4.716
  
  Portrat, S., Guida, A., Phénix, T., & Lemaire, B. (2016). Promoting the experimental
  dialogue between working memory and chunking: Behavioral data and simulation. 
  \emph{Memory & Cognition, 44}, 420-434. http://dx.doi.org/10.3758/s13421-015-0572-9
  
  Quinlan, P.T., Roodenrys, S. & Miller, L.M. (2017). Serial reconstruction of order and
  serial recall in verbal short-term memory. \emph{Memory & Cognition, 45}(7), 1126-1143. 
  https://doi.org/10.3758/s13421-017-0719-y
  
  Thalmann, M., Souza, A. S., & Oberauer, K. (2019). How does chunking help working memory?
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 45}(1), 37-55.
  https://doi.org/10.1037/xlm0000578
  
  
}

\examples{
  data(page06)
  ?portrat16
}


