\name{peteranderl17}
\alias{peteranderl17}
\docType{data}
\title{
  Peteranderl & Oberauer's (2017) data on the primacy effect in continuous color
  reproduction with and without articulatory suppression (AS)
}
\description{
  Individual-trial data from Peteranderl & Oberauer (2017). Participants contnuously
  reproduced colors with or without articulatory suppression during presentation.
  The data show a primacy effect in the precision measure.
}
\usage{data("peteranderl17")}
\format{
  A data frame with 50400 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{AS}}{Articulatory Suppression: \itemize{\item{0=silent}\item{1=AS}}}
    \item{\code{serpos}}{Serial Position}
    \item{\code{errordeg}}{Precision error in degrees}
  }
}
\details{
  For details, see Peteranderl & Oberauer (2017)
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Peteranderl, S., & Oberauer, K. (2017). Serial recall of colors: Two models of
  memory for serial order applied to continuous visual stimuli. \emph{Memory &
  Cognition, 46}, 1-16. http://dx.doi.org/10.3758/s13421-017-0741-0
  
}
\examples{
data(peteranderl17)
## Reproducing Figure 4a in Peteranderl & Oberauer (2017)
pd <- aggregate(errordeg ~ AS*serpos, data = peteranderl17, FUN=mean)
plot(c(1,5), c(0.0,65), type = "n", xlab = "Serial Position",
     ylab = "Precision error in degrees", 
     main = "Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5), labels = T)

lines(x = pd$serpos[pd$AS == 0], 
      y = pd$errordeg[pd$AS == 0], 
      type = "l", lty = 1)
lines(x = pd$serpos[pd$AS == 1], 
      y = pd$errordeg[pd$AS == 1], 
      type = "l", lty = 2)
legend(1, 65, c("silent", "AS"), lty = 1:2,
       horiz = F, cex = 0.5, yjust = 1, xjust = 0)

}
\keyword{datasets}
