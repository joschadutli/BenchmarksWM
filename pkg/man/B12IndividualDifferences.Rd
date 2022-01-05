\name{B12IndividualDifferences}
\alias{B12IndividualDifferences}
\alias{B12 IndividualDifferences}
\alias{IndividualDifferences}

\title{Working Memory Benchmarks 12: Individual Differences in Working Memory Capacity}

\description{
  Subsection of the Working Memory (WM) Benchmarks datasets (Oberauer, 2019) associated
  with individual differences in Working Memory capacity (WMC). This documentation summarizes 
  the subsections on the correlations to many other cognitive abilities (i.e. WM's positive
  manifold), the seperation of primary and secondary memory, and the correlation between
  verbal WMC and processing speed in articulation and retrieval.
  See Oberauer et al. (2018; 912ff.) for a more precise description of the Benchmarks.
}

\format{
  \strong{3 datasets on individual differences in WM and WMC.}
  \describe{
  \item{\code{\link{kane04}}}{Correlations of WM performance and WMC to short-term memory and reasoning tasks. (BM 12.1)}
  \item{\code{\link{unsworth10}}}{The separation of primary and secondary memory components, both of which explain unique variance in WMC. (BM 12.4)}
  \item{\code{\link{cowan98}}}{Correlation between processing speed in articulation and retrieval and WMC. (BM 12.5)}
  }
}

\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Cowan, N., Wood, N. L., Wood, P. K., Keller, T. A., Nugent, L. D., & Keller, 
  & C. V. (1998). Two separate  verbal  processing  rates  contributing  to  
  short-term memory  span.  \emph{Journal  of Experimental Psychology: General, 
  127}, 141–160. 10.1037//0096-3445.127.2.141 
  
  Kane, M. J., Hambrick, D. Z., Tuholski, S. W., Wilhelm, O., Payne, T. W., & Engle, R. W.
  (2004). The generality of working memory capacity: A latent-variable approach to verbal 
  and visuospatial memory span and reasoning. \emph{Journal of Experimental Psychology:
  General, 133}, 189– 217. http://dx.doi.org/10.1037/0096-3445.133.2.189
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Unsworth, N., Spillers, G. J., & Brewer, G. A. (2010). The contributions of primary 
  and secondary memory to working memory capacity: An individual differences analysis 
  of immediate free recall. \emph{Journal of Experimental Psychology: Learning, Memory, 
  and Cognition, 36}, 240–247. http://dx.doi.org/10.1037/a0017739
  
  
}

\examples{
  data(kane04)
  ?unsworth10
}
