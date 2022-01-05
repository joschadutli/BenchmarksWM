\name{B8SimilarityEffects}
\alias{B8SimilarityEffects}
\alias{B8 SimilarityEffects}
\alias{SimilarityEffects}

\title{Working Memory Benchmark 8: Effects of Similarity}

\description{
  Subsection of the Working Memory (WM) Benchmarks datasets (Oberauer, 2019) associated
  with the effects of similarity. This documentation summarizes the subsections of 
  Benchmark 8 in Oberauer et al. (2018) and contains data on the effects of phonological
  similarity in verbal WM as well as data on the effects of item-probe similarity in 
  change detection tasks in visual WM.
  See Oberauer et al. (2018; 906f.) and Bhatarah et al. (2009) for a more precise 
  description of the datasets and the Benchmark findings they represent.
}

\format{
  \strong{4 datasets on the effects of similarity in verbal and visual WM.}
  \describe{
  \item{\code{\link{farrell03}}}{Effects of phonologically similar consonants in serial recall. (BM 8.1)}
  \item{\code{\link{jarrold13}}}{Effects of phonologically similar words in serial and free recall in children. (BM 8.1)}
  \item{\code{\link{macnamara11}}}{Phonological Similarity Effects in complex span tasks. (BM 8.1)}
  \item{\code{\link{keshvari13}}}{Effects of Item-Probe Similarity in a Change Detection task in visual WM. (BM 8.2)}
  }
}

\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}

\references{
  
  Farrell, S., & Lewandowsky, S. (2003). Dissimilar items benefit from phonological 
  similarity in serial recall. \emph{Journal of Experimental Psychology: Learning, Memory, 
  and Cognition, 29}, 838–849. http://dx.doi.org/10.1037/0278-7393.29.5.838
  
  Jarrold, C., & Citroën, R. (2013). Reevaluating key evidence for the development of
  rehearsal: Phonological similarity effects in children are subject to proportional 
  scaling artifacts. \emph{Developmental Psychology, 49}, 837–847. 
  http://dx.doi.org/10.1037/a0028771
  
  Keshvari, S., van den Berg, R., & Ma, W. J. (2013). No evidence for an item limit in 
  change detection. \emph{PLoS Computational Biology, 9}, e1002927. 
  http://dx.doi.org/10.1371/journal.pcbi.1002927
  
  Macnamara, B. N., Moore, A. B., & Conway, A. R. A. (2011). Phonological similarity 
  effects in simple and complex span tasks. \emph{Memory & Cognition, 39}, 1174–1186. 
  http://dx.doi.org/10.3758/s13421-011-0100-5
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  
}

\examples{
  data(keshvari13)
  ?macnamara11
}

