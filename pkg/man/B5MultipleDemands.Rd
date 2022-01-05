\name{B5MultipleDemands}
\alias{B5MultipleDemands}
\alias{B5 MultipleDemands}
\alias{MultipleDemands}

\title{Working Memory Benchmark 5: Effects of Combining Multiple Demands}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with the effects of combining multiple demands. This documentation summarizes the 
  subsections of Benchmark 5 in Oberauer et al. (2018) and contains data on the 
  effects of multiple tasks and multiple stimulus domains. Additionally, there's data
  on the cognitive load effect in complex span tasks.
  See Oberauer et al. (2018; 902-906) for a more precise description of the 
  datasets and the Benchmark findings they represent.
}

\format{
  \strong{9 datasets on combining multiple demands in WM tasks.}
  \describe{
  \item{\code{\link{cowan07}}}{The effects of combining multiple memory sets from the same or different stimulus domains (BM 5.1.1)}
  \item{\code{\link{chein11}}}{The effects of performing multiple tasks. Data from a complex span task in different stimulus domains (BM 5.2)}
  \item{\code{\link{jarrold10}}}{Position effects of performing a processing task between presentation of items of a working memory task (BM 5.2)}
  \item{\code{\link{klauer04}}}{Double Dissociation between visual and spatial working memory (BM 5.2)}
  \item{\code{\link{barrouillet07}}}{Cognitive Load effect in complex span tasks in working memory (BM 5.2.4)}
  \item{\code{\link{barrouillet11}}}{Cognitive Load effects in different working memory tasks (BM 5.2.4)}
  \item{\code{\link{vergauwe10}}}{Cognitive Load effects and the effects of stimulus domain of processing tasks (BM 5.2.4)}
  \item{\code{\link{vergauwe12}}}{Data on resource-sharing between processes in tasks involving different stimulus domains (BM 5.2.4)}
  \item{\code{\link{vergauwe15}}}{Cognitive load effects in verbal and visual recognition memory (BM 5.2.4)}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Barrouillet, P., Bernardin, S., Portrat, S., Vergauwe, E., & Camos, V. (2007). Time 
  and cognitive load in working memory. \emph{Journal of Experimental Psychology: Learning,
  Memory, and Cognition, 33}, 570–585. http://dx.doi.org/10.1037/0278-7393.33.3.570
  
  Barrouillet, P., Portrat, S., & Camos, V. (2011). On the law relating processing to 
  storage in working memory. \emph{Psychological Review, 118}, 175–192. 
  http://dx.doi.org/10.1037/a0022324
  
  Chein, J. M., Moore, A. B., & Conway, A. R. A. (2011). Domain-general mechanisms of
  complex working memory span. \emph{NeuroImage, 54}, 550–559. 
  http://dx.doi.org/10.1016/j.neuroimage.2010.07.067
  
  Cowan, N., & Morey, C. C. (2007). How can dual-task working memory retention limits 
  be investigated? \emph{Psychological Science, 18}, 686–688. 
  http://dx.doi.org/10.1111/j.1467-9280.2007.01960.x
  
  Jarrold, C., Tam, H., Baddeley, A. D., & Harvey, C. E. (2010). The nature and position 
  of processing determines why forgetting occurs in working memory tasks. \emph{Psychonomic
  Bulletin & Review, 17}, 772–777. http://dx.doi.org/10.3758/PBR.17.6.772
  
  Klauer, K. C., & Zhao, Z. (2004). Double dissociations in visual and spatial short-term
  memory. \emph{Journal of Experimental Psychology: General, 133}, 355–381. 
  http://dx.doi.org/10.1037/0096-3445.133.3.355
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Vergauwe, E., Barrouillet, P., & Camos, V. (2010). Do mental processes share a 
  domain-general resource? \emph{Psychological Science, 21}, 384–390. 
  http://dx.doi.org/10.1177/0956797610361340
  
  Vergauwe, E., Dewaele, N., Langerock, N., & Barrouillet, P. (2012). Evidence for a central
  pool of general resources in working memory. \emph{Journal of Cognitive Psychology, 24},
  359–366. http://dx.doi.org/10.1080/20445911.2011.640625
  
  Vergauwe, E., Hartstra, E., Barrouillet, P., & Brass, M. (2015). Domain-general involvement
  of the posterior frontolateral cortex in Time-Based Resource-Sharing in working memory: An
  fMRI study. \emph{NeuroImage, 115}, 104–116. http://dx.doi.org/10.1016/j.neuroimage.2015.04.059
  
}

\examples{
  data(vergauwe10)
  ?chein11
}
