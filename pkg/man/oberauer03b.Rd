\name{oberauer03b}
\alias{oberauer03b}
\docType{data}
\title{
  Oberauer's (2003) Data on Serial Position Curves
}
\description{
  Individual-subject data from Oberauer (2003) on serial position curves, listed 
  as a Benchmark in Oberauer et al. (2018) and archived by Oberauer (2019). 
  One group of participants performed a global recognition task and another a local
  recognition task, in which participants memory performance depended on relational
  memory to the spatial position in addition the item memory that is necessary in 
  the global recognition task. Within subjects, the order of presentation was 
  varied. In the forward condition, items were presented from left to right
  and tested from left to write. In the random condition, the spatial order of
  presentation was randomized as well as the temporal order at test. In this
  condition, the temporal and spatial serial positions at presentation and at test
  were deconfounded. 
}
\usage{data("oberauer03b")}
\format{
  A data frame with 636 observations on the following 10 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant in each group.}
    \item{\code{memory}}{Between-subjects (group): Memory task: \code{local} or \code{global}.}
    \item{\code{cond}}{Within-subjects condition: \code{forward} or \code{random}}
    \item{\code{serpos}}{Serial Position (1 - 6)}
    \item{\code{in_acc}}{Proportion correct at this Input Position}
    \item{\code{spatial_acc}}{Proportion correct at this Spatial Position}
    \item{\code{out_acc}}{Proportion correct at this Output Position}
    \item{\code{in_rt}}{Response time (ms) at this Input Position}
    \item{\code{spatial_rt}}{Response time (ms) at this Spatial Position}
    \item{\code{out_rt}}{Response time (ms) at this Output Position}
  }
}
\details{
  The variable \code{serpos} in combination with the different accuracy (proportion
  correct) and response time measures, codes the temporal input, spatial (in- and 
  output) and temporal output positions. For plots or analyses, combine 
  \code{serpos} with the input accuracy measure \code{in_acc}, for example, and 
  read \code{serpos} as the temporal input position.
  For more details, see Oberauer (2003).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}
\references{
  Oberauer, K. (2003). Understanding serial position curves in short-term 
  recognition and recall. \emph{Journal of Memory and Language, 49}, 469-483. 
  http://dx.doi.org/10.1016/S0749-596X(03)00080-9
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
  
}
\examples{
data(oberauer03b)
## Reproduce Figure 7B in Oberauer et al. (2018)
pd <- oberauer03b[which(oberauer03b$cond == "random"),]
agg_pd <- aggregate(in_acc ~ serpos*memory, data = pd, FUN = mean)
plot(c(1,6), c(0.5,1.0), type = "n", xlab = "Serial Position (Input)",
     ylab = "Proportion correct", 
     main = "Serial Position Curve in Recognition Memory", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = TRUE)

lines(x = agg_pd$serpos[agg_pd$memory == "global"], 
      y = agg_pd$in_acc[agg_pd$memory == "global"], 
      type = "l", lty = 1)
lines(x = agg_pd$serpos[agg_pd$memory == "local"], 
      y = agg_pd$in_acc[agg_pd$memory == "local"], 
      type = "l", lty = 1)
points(x = agg_pd$serpos[agg_pd$memory == "global"], 
       y = agg_pd$in_acc[agg_pd$memory == "global"],  
       pch = 21, bg = "black")
points(x = agg_pd$serpos[agg_pd$memory == "local"], 
       y = agg_pd$in_acc[agg_pd$memory == "local"],  
       pch = 22, bg = "grey")
legend(1.0, 1, c("Item Recognition", "Relational Recognition"), lty = 1, 
       pch = 21:22, pt.bg = c("black","grey"), horiz = FALSE, cex = 0.8, yjust = 1, xjust = 0)
}
\keyword{datasets}
