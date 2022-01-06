\name{B1.2 SetsizeRT}
\alias{SetsizeRT}
\alias{B1.2 SetsizeRT}
\alias{B1.2SetsizeRT}

\title{Working Memory Benchmarks: Set Size Effects on Response Times}

\description{
  Subsection of the Working Memory Benchmarks datasets (Oberauer, 2019) associated
  with set size effects on response time measures. Overall, the data show a
  monotonic increase in response times with increasing set size. See section
  "Details" for more precise descriptions of the datasets.
}
\details{
  Benchmark datasets in Working Memory of set size effects on response times
  as listed by Oberauer et al. (2018) and archived by Oberauer (2019). 
  \itemize{
    \item{Donkin & Nosofsky's (2012b) data show the effects of serial position
  and/or set size on response times when items are presented at a slow
  vs. fast rate.}
    \item{Gilchrist & Cowan's (2014) data show a monotonic increase in response
    times with increasing set size in change detection.}
    \item{Shepherdson, Oberauer, & Souza (2018) report data on the interacting effect of retro cues on the monotonic increase of response times with larger set sizes. A large part of the dataset were collected and partially analyzed by Souza, Rerko, & Oberauer (2014).}
    \item{Towse, Cowan, Hitch, & Horton's (2008) data show an increase in response
  times for longer lists in a complex span task.}
  }
}
\format{
  \strong{4 datasets with set size effects on response times.}
  \describe{
  \item{\code{\link{donkin12b}}}{Set size and serial position effects
  on response times for fast vs. slow presentation rates in the Sternberg
  task.}
  \item{\code{\link{gilchrist14}}}{Set size effects on response times
  in a change detection task.}
  \item{\code{\link{shepherdson18}}}{Effects of retro cues and set size on
  response times in recognition memory with data collected by Souza, Rerko, & Oberauer (2014).}
  \item{\code{\link{towse08}}}{Effects of list length on response times in a 
  complex span task.}
  }
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Donkin, C., & Nosofsky, R. M. (2012b). The structure of short-term memory
  scanning: An investigation using response time distribution models. \emph{Psychonomic
  Bulletin & Review, 19}, 363-394. http://dx.doi.org/ 10.3758/s13423-012-0236-8
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  Shepherdson, P., Oberauer, K., & Souza, A. S. (2018). Working Memory Load
  and the Retro-Cue Effect: A Diffusion Model Account. \emph{Journal of Experimental
  Psychology: Human Perception and Performance, 44}, 286-310. 
  http://dx.doi.org/10.1037/xhp0000448
  
  Souza, A. S., Rerko, L., & Oberauer, K. (2014). Unloading and reloading working
  memory: Attending to one item frees capacity. \emph{Journal of Experimental 
  Psychology: Human Perception and Performance, 40}, 1237-1256.
  http://dx.doi.org/10.1037/a0036331
  
  Sternberg, S. (1966). High-speed scanning in human memory. \emph{Science, 153}, 652-654.
  http://dx.doi.org/10.1126/science.153.3736.652
  
  Towse, J. N., Cowan, N., Hitch, G. J., & Horton, N. J. (2008). The recall of
  information from working memory. Insights from behavioural and chronometric
  perspectives. \emph{Experimental Psychology, 55}, 371-383. 
  http://dx.doi.org/10.1027/1618-3169.55.6.371
  
}
\examples{
  data(donkin12b)
  ?donkin12b
}
