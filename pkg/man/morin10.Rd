\name{morin10}
\alias{morin10}
\docType{data}
\title{
  Morin, Brown, & Lewandowsky's (2010) Data on Temporal Isolation Effects in Recognition
}
\description{
  These are the individual-trial data from Experiment 1 in Morin, Brown, and Lewandowsky
  (2010). Intervals between two items in a memory list were manipulated experimentally to
  produce temporal isolation for certain items of the list. Temporal isolation resulted in 
  a larger probability that participants remembered the correct word in a recognition test.
}
\usage{data("morin10")}
\format{
  A data frame with 1296 observations on the following 12 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{age}}{Age}
    \item{\code{sex}}{Sex}
    \item{\code{trial}}{Unique trial ID}
    \item{\code{totaltime}}{Total time of presentation phase}
    \item{\code{iso.time}}{Temporal isolation: sum of pre- and postitem gaps}
    \item{\code{bin}}{Collapsed temporal isolation for Figure 2 in Morin et al. (2010)}
    \item{\code{serpos}}{Serial Position (1-9)}
    \item{\code{probe}}{Recognition probe: "yes" for positive probe (see Details)}
    \item{\code{resp}}{Response: \code{p} = "new item"; \code{r} = "old item" (remember);  \code{u} = "old item" (know) (see Morin et al., 2010)}
    \item{\code{acc}}{Accuracy: 1 = correct, 0 = incorrect}
    \item{\code{rt}}{Response times (ms)}
  }
}
\details{
  For details about the nature of the responses available for participants, see Morin,
  Brown, and Lewandowsky (2010).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Morin, C., Brown, G. D. A., & Lewandowsky, S. (2010). Temporal isolation effects in
  recognition and serial recall. \emph{Memory & Cognition, 38}, 849–859. http://dx.doi.org/10.3758/MC.38.7.849
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Approximately reproduce Figure 2 in Morin et al. (2010).
data(morin10)
## Exclude serial positions at beginning and end of list
twoseven <- morin10[which(morin10$serpos %in% c(2,3,4,5,6,7)),]

pd <- plyr::count(twoseven, vars = c("resp","bin"))
pd <- tidyr::spread(pd, key = "resp", value = "freq")

pd$tot <- pd$r+pd$u+pd$p
pd$score <- pd$r/pd$tot
pd$uscore <- pd$u/pd$tot
pd$bin <- pd$bin/1000

plot(c(0,10), c(0,0.8), type = "n", xlab = "Combined Temporal Isolation (sec)",
     ylab = "Proportion correct", 
     main = "Figure 2 in Morin et al. (2010)", xaxt = "n")
axis(side = 1, at = c(0,2,4,6,8,10), labels = c(0,2,4,6,8,10), 
     cex.axis = 1.0)
points(x = pd$bin, y = pd$score, 
       type = "p", lty = 1, pch = 15)
abline(lm(score ~ bin, data = pd), lty = 1, pch = 15)
points(x = pd$bin, y = pd$uscore, 
       type = "p", lty = 2, pch = 17)
abline(lm(uscore ~ bin, data = pd), lty = 2, pch = 17)
legend(10, 0, c("remember", "know"), 
       lty = 1:2, 
       pch=c(15,17), 
       horiz = F, cex = 0.9, yjust = 0, xjust = 1)
}
\keyword{datasets}
