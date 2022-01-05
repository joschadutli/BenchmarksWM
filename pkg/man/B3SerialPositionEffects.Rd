\name{B3SerialPositionEffects}
\alias{B3SerialPositionEffects}
\alias{B3 SerialPositionEffects}
\alias{SerialPositionEffects}

\title{Working Memory Benchmarks 3: Effects of Serial Position}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with the effects of serial position. This documentation summarizes the 
  subsections of Benchmark 3 in Oberauer et al. (2018) and contains primacy and
  recency effects, modality effects, effects of serial position on latencies and 
  effects of output order.
  See Oberauer et al. (2018; 898ff.) for a more precise description of the 
  datasets and the Benchmark findings they are associated with.
}

\format{
  \strong{11 datasets with effects of serial position on accuracy and/or retrieval latencies.}
  \describe{
  \item{\code{\link{madigan71}}}{Primacy and recency effects on accuracy in verbal
  serial recall. (BM 3.1)}
  \item{\code{\link{oberauer03b}}}{Serial Position effects of input and output positions.
  (BM 3.1, BM 3.4)}
  \item{\code{\link{peteranderl17}}}{Primacy effects in visual serial recall of colors. 
  (BM 3.1)}
  \item{\code{\link{harvey07}}}{Modality (visual vs. auditory) and recency effects
  in serial recall. (BM 3.2)}
  \item{\code{\link{oeztekin10}}}{Fast access to recently presented items. (BM 3.3.2)}
  \item{\code{\link{farell04}}}{Serial position effects on retrieval latencies.
  (BM 3.3.3)}
  \item{\code{\link{murdock70}}}{Output position effect on inter-response latencies (BM 3.3.3)}
  \item{\code{\link{cowan02}}}{Deconfounded output and input position effects in serial recall. (BM 3.4.1)}
  \item{\code{\link{lange11}}}{Effects of output contiguity. Better accessibility of recognition probes when probed in forward order in comparison to random order. (BM 3.4.3)}
  \item{\code{\link{grenfell12}}}{Effects of self-chosen output order in free recall. (BM 3.5.1)}
  \item{\code{\link{healey14}}}{Semantic clustering in free recall (BM 3.5.2)}
  }
}


\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Cowan, N., Saults, J. S., Elliott, E. M., & Moreno, M. V. (2002). 
  Deconfounding serial recall. \emph{Journal of Memory and Language, 46}, 153–177.
  http://dx.doi.org/10.1006/jmla.2001.2805
  
  Farrell, S., & Lewandowsky, S. (2004). Modelling transposition latencies: 
  Constraints for theories of serial order memory. \emph{Journal of Memory and
  Language, 51}, 115–135. http://dx.doi.org/10.1016/j.jml.2004.03.007

  Grenfell-Essam, R., & Ward, G. (2012). Examining the relationship between free
  recall and immediate serial recall: The role of list length, strategy use,
  and test expectancy. \emph{Journal of Memory and Language, 67}, 106–148. 
  http://dx.doi.org/10.1016/j.jml.2012.04.004
  
  Harvey, A. J., & Beaman, C. P. (2007). Input and output modality effects in
  immediate serial recall. \emph{Memory, 15}, 693–700.
  http://dx.doi.org/10.1080/09658210701644677
  
  Healey, M. K., Crutchley, P., & Kahana, M. J. (2014). Individual differences 
  in memory search and their relation to intelligence. \emph{Journal of 
  Experimental Psychology: General, 143}(4), 1553. https://doi.org/10.1037/a0036306
  
  Lange, E. B., Cerella, J., & Verhaeghen, P. (2011). Ease of access to list items 
  in short-term memory depends on the order of the recognition probes. \emph{Journal
  of Experimental Psychology: Learning, Memory, and Cognition, 37}, 608–620. 
  http://dx.doi.org/10.1037/a0022220
  
  Madigan, S. A. (1971). Modality and recall order interactions in short-term memory
  for serial order. \emph{Journal of Experimental Psychology, 87}, 294– 296.
  
  Murdock, B. B., & Okada, R. (1970). Interresponse times in single-trial free 
  recall. \emph{Journal of Experimental Psychology, 86}, 263–267. 
  http://dx.doi.org/10.1037/h0029993
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Öztekin, I., & McElree, B. (2010). Relationship between measures of working 
  memory capacity and the time course of short-term memory retrieval and 
  interference resolution. \emph{Journal of Experimental Psychology: Learning, 
  Memory, and Cognition, 36}, 383–397. http://dx.doi.org/10.1037/a0018029
  
  Peteranderl, S., & Oberauer, K. (2017). Serial recall of colors: Two models of
  memory for serial order applied to continuous visual stimuli. \emph{Memory &
  Cognition, 46}, 1–16. http://dx.doi.org/10.3758/s13421-017-0741-0
  
}
\examples{
  data(madigan71)
  ?peteranderl17
}





