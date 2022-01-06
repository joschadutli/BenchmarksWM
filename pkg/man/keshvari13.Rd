\name{keshvari13}
\alias{keshvari13}
\docType{data}
\title{
  Keshvari, van den Berg, & Ma's (2013) Data on Item-Probe Similarity in Change Detection in
  Visual Working Memory
}
\description{
  These are the individual-trial data from Keshvari et al. (2013). Stimuli were oriented bars
  on the screen. Set size and the size of change to be detected were varied across trials.
  Detecting small changes (when item-probe similarity is high) is more challenging than 
  detecting large changes. This pattern can be observered for different set sizes.
}
\usage{data("keshvari13")}
\format{
  A data frame with 18000 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{setsize}}{Set size (2, 4, 6, or 8)}
    \item{\code{delta}}{Size of Change (rad)}
    \item{\code{deltabin}}{Bins of changes that can be used to plot (rad)}
    \item{\code{response}}{Response: \code{1} = "change"; \code{0} = "same"}
  }
}
\details{
  For further details, see Keshvari, van den Berg, and Ma (2013).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  
  Keshvari, S., van den Berg, R., & Ma, W. J. (2013). No evidence for an item limit in 
  change detection. \emph{PLoS Computational Biology, 9}, e1002927. 
  http://dx.doi.org/10.1371/journal.pcbi.1002927
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Approximate reproduction of Figure 2c in Keshvari et al. (2013)
## and Figure B3 in Oberauer et al. (2018)

data(keshvari13)
pd <- aggregate(response ~ deltabin+setsize, data = keshvari13, FUN = mean)

plot(c(0,1.7), c(0,1.0), type = "n", xlab = "Size of Change (rad)",
     ylab = "Proportion \"change\"", 
     main = "Change Detection in Visual WM", xaxt = "n")
axis(side = 1, at = c(0.0,0.5,1.0,1.5), labels = c(0.0,0.5,1.0,1.5), 
     cex.axis = 1.0)
lines(x = pd$deltabin[pd$setsize == 2], 
      y = pd$response[pd$setsize == 2], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$deltabin[pd$setsize == 4], 
      y = pd$response[pd$setsize == 4], 
      type = "b", lty = 2, pch = 16, col = "darkgrey")
lines(x = pd$deltabin[pd$setsize == 6], 
      y = pd$response[pd$setsize == 6], 
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$deltabin[pd$setsize == 8], 
      y = pd$response[pd$setsize == 8], 
      type = "b", lty = 4, pch = 18, col = "lightgrey")
legend(1.6, 0, c("N=1","N=2","N=4","N=8"), lty = 1:4, pch=15:18,
       col = c("black", "darkgrey", "grey","lightgrey"), horiz = F, cex = 0.6,
       yjust = 0, xjust = 1)
}
\keyword{datasets}
