\name{schlittmeier12}
\alias{schlittmeier12}
\docType{data}
\title{
  Schlittmeier's et al. (2012) Data on the Irrelevant Sound Effect
}
\description{
  Individual-trial data from Schlittmeier, Weissgerber, Kerber, Fastl, and Hellbrück (2012).
  Participants performed a verbal working memory task in which they were asked to serially
  recall a list of digits in correct order. To induce the Irrelevant Sound Effect (ICE), 
  different irrelevant sounds accompanied the task in the experimental conditions. In the
  control condition, participants performed the task in silence. The data consist of two 
  experiments (or a between-subjects manipulation) that differed in the types of irrelevant
  sounds present. For speech as sounds, the speech was either changing or steady (or quiet). 
  For music as sounds, the music was either staccato or legato (or quiet).
}
\usage{data("schlittmeier12")}
\format{
  A data frame with 1350 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant for each type of sound}
    \item{\code{type}}{Type of sound}
    \item{\code{condition}}{Conditions: \code{quiet}, \code{changing}, \code{steady}, \code{staccato}, \code{legato}}
    \item{\code{serpos}}{Serial position}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{
  For further details, see Schlittmeier et al. (2012).
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
  
  Schlittmeier, S. J., Weißgerber, T., Kerber, S., Fastl, H., & Hellbrück, J. (2012).
  Algorithmic modeling of the irrelevant sound effect (ISE) by the hearing sensation 
  fluctuation strength. \emph{Attention, Perception & Psychophysics, 74}, 194-203. 
  http://dx.doi.org/10.3758/s13414-011-0230-7
}
\examples{
### Reproduce left panel of Figure 15 in Oberauer et al. (2018)
data(schlittmeier12)
pdd <- aggregate(acc ~ serpos+type+condition, data = schlittmeier12, FUN = mean)
pd <- subset(pdd, type == "speech")
plot(c(0,10), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Irrelevant speech and sound effects", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = levels(pd$condition), cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "quiet"], y = pd$acc[pd$condition == "quiet"], 
      type = "b", lty = 1, pch = 15, col = "red")
lines(x = pd$serpos[pd$condition == "steady"], y = pd$acc[pd$condition == "steady"], 
      type = "b", lty = 2, pch = 17, col = "blue")
lines(x = pd$serpos[pd$condition == "changing"], y = pd$acc[pd$condition == "changing"], 
      type = "b", lty = 3, pch = 18, col = "green")
legend(0, 0, c("quiet", "steady", "changing"), lty = 1:3, pch=c(15,17, 18), 
       title = "Condition:",
       col = c("red", "blue", "green"), horiz = F, cex = 0.6, yjust = 0, xjust = 0)
}
\keyword{datasets}
