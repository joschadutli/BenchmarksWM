\name{ricker17}
\alias{ricker17}
\docType{data}
\title{
  Ricker and Hardman's (2017) Data on the Effect of Consolidation Time on Presicion in a visual Working Memory Task
}
\description{
 Individual-trial data from Ricker and Hardman (2017). Participants had to recall 
 the position of a dot on a ring. Memory items were presented sequentially. 
 Consolidation time varied randomly between 17 and 1000 ms but was held constant 
 within one trial. Consolidation time had a significant beneficial effect on the 
 precision measure for memory items except for the first item presented in a trial, 
 which was recalled with less presicion when consolidation time increased.
}
\usage{data("ricker17")}
\format{
  A data frame with 18000 observations on the following 7 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{block}}{Block ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{ctime}}{Condolidation time in seconds}
    \item{\code{serpos}}{Serial position of the memory item}
    \item{\code{error}}{Precision error in degrees [-180, 180]}
    \item{\code{rt}}{Response time in seconds}
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
  
  Ricker, T. J., Hardman, K. O. (2017). The Nature of Short-Term Consolidation
  in Visual Working Memory. \emph{Journal of Experimental Psychology: General, 146,
  11}, 1551-1573. http://dx.doi.org/10.1037/xge0000346
}
\examples{
data(ricker17)

### Vague reproduction of Figure 5 in Ricker & Hardman (2017)
ricker17$error <- abs(ricker17$error)
dp <- aggregate(error ~ serpos*ctime, data = ricker17, FUN = mean)

par(mfrow=c(1,1))
plot(c(0.2,1.0), c(0,55), type = "n", xlab = "Condolidation time in seconds",
     ylab = "Precision error in degrees", 
     main = "Serial Position and Consolidation Time", xaxt = "n")
axis(side = 1, at = c(0.2,0.4,0.6,0.8,1.0), labels = TRUE)

lines(x = dp$ctime[dp$serpos == 1], 
      y = dp$error[dp$serpos == 1], 
      type = "l", lty = 1)
lines(x = dp$ctime[dp$serpos == 2], 
      y = dp$error[dp$serpos == 2], 
      type = "l", lty = 2)
lines(x = dp$ctime[dp$serpos == 3], 
      y = dp$error[dp$serpos == 3], 
      type = "l", lty = 3)
lines(x = dp$ctime[dp$serpos == 4], 
      y = dp$error[dp$serpos == 4], 
      type = "l", lty = 4)
legend(1.0, 0, c("1", "2", "3", "4"), lty = 1:4,
       title = "Serial Position", horiz = F, cex = 0.5, yjust = 0, xjust = 1)

}
\keyword{datasets}
