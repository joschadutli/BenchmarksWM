\name{B4ErrorCharacteristics}
\alias{B4ErrorCharacteristics}
\alias{B4 ErrorCharacteristics}
\alias{ErrorCharacteristics}

\title{Working Memory Benchmark 4: Characteristics of Errors}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with the effects of error characteristics. This documentation summarizes the 
  subsections of Benchmark 4 in Oberauer et al. (2018) and contains data on the 
  locality constraint, the fill-in effect and set size related error distributions
  in continuous reproduction tasks in visual WM.
  See Oberauer et al. (2018; 900ff.) for a more precise description of the 
  datasets and the Benchmark findings they are associated with.
}


\format{
  \strong{4 datasets on error characteristics in WM tasks.}
  \describe{
  \item{\code{\link{farrell04}}}{The locality constraint. Transposition errors in serial
  recall. (BM 4.1.1)}
  \item{\code{\link{rerko14}}}{The locality constraint. Error probabilities constrained
  by the Euclidean distance of items on the screen. (BM 4.4.1)}
  \item{\code{\link{farrell13}}}{The fill-in effect: Serial Position errors and their 
  consequences (BM 4.1.2)}
  \item{\code{\link{vandenberg12}}}{Error distributions on continuous response scales in 
  visual WM (BM 4.5)}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Farrell, S., Hurlstone, M. J., & Lewandowsky, S. (2013). Sequential dependencies in recall
  of sequences: Filling in the blanks. \emph{Memory & Cognition, 41}, 938-952. 
  http://dx.doi.org/10.3758/s13421-013-0310-0
  
  Farrell, S., & Lewandowsky, S. (2004). Modelling transposition latencies: 
  Constraints for theories of serial order memory. \emph{Journal of Memory and
  Language, 51}, 115-135. http://dx.doi.org/10.1016/j.jml.2004.03.007
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Rerko, L., Oberauer, K., & Lin, H.-Y. (2014). Spatially imprecise representations in 
  working memory. \emph{Quarterly Journal of Experimental Psychology: Human Experimental
  Psychology, 67}, 3-15. http://dx.doi.org/10.1080/17470218.2013.789543
  
  van den Berg, R., Shin, H., Chou, W.-C., George, R., & Ma, W. J. (2012). Variability in
  encoding precision accounts for visual short-term memory limitations. \emph{Proceedings of
  the National Academy of Sciences of the United States of America, 109}, 8780-8785. 
  http://dx.doi.org/10.1073/pnas.1117465109
  
}

\examples{
  data(rerko14)
  ?vandenberg12
}


