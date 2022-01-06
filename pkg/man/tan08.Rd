\name{tan08}
\alias{tan08}
\docType{data}
\title{
  Tan and Ward's (2008) Data on the Effects of Presentation Duration and Serial
  Position in Verbal Working Menmory
}
\description{
  Individual-trial data from Tan and Ward's (2008) experiment on immediate 
  serial recall of six words with different presentation duration. Participants 
  engaged in overt rehearsal in the free time during presentation. More free time
  between the presentation of two words leads to better serial-recall performance.
}
\usage{data("tan08")}
\format{
  A data frame with 5760 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial IDs}
    \item{\code{ptime}}{Presentation time per item:
    \itemize{
    \item{\code{1} s / item} \item{\code{2.5} s / item} \item{\code{5} s / item}
    }}
    \item{\code{serpos}}{Serial Position: \code{1-6}}
    \item{\code{corr}}{Correct: \itemize{\item{\code{1} = correct}
    \item{\code{0} = incorrect}}}
  }
}
\details{
  These are the individual-trial data from Tan & Ward (2008), listed as a benchmark
  dataset by Oberauer et al. (2018) and archived by Oberauer (2019). The data show 
  better immediate serial recall performance for longer presentation durations in a
  verbal Working Memory task. The presentation duration was varied between 1 second, 
  2.5 seconds and 5 seconds per item.
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
  
  Tan, L., & Ward, G. (2008). Rehearsal in immediate serial recall. \emph{Psychonomic
  Bulletin & Review, 15}, 535-542. http://dx.doi.org/10.3758/PBR.15.3.535
  
}
\examples{
data(tan08)

### Reproduce Figure 6B in Oberauer et al. (2018)
plotd <- aggregate(corr ~ serpos*ptime, data = tan08, FUN = mean)
plot(c(1,6), c(0,1), type = "n", xlab = "Serial Position",
     ylab = "Proportion Correct", main = "Serial Recall as an Effect of 
     Presentation Timing", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6), labels = T)
lines(x = plotd$serpos[plotd$ptime == 1], y = plotd$corr[plotd$ptime == 1],
      type = "b", lty = 1, pch = 15)
lines(x = plotd$serpos[plotd$ptime == 2.5], y = plotd$corr[plotd$ptime == 2.5],
      type = "b", lty = 2, pch = 16)
lines(x = plotd$serpos[plotd$ptime == 5], y = plotd$corr[plotd$ptime == 5],
      type = "b", lty = 3, pch = 17)
legend(4, 0.6, c("1 s / item", "2.5 s / item", "5 s / item"), lty = 1:3,
       pch=15:17, yjust=0)

}
\keyword{datasets}
