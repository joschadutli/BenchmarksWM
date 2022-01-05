\name{bell19}
\alias{bell19}
\docType{data}
\title{
  Bell's et al. (2019) Data on the Changing-State and the Auditory Deviant Effect
}
\description{
  Individual-trial data from a large preregistered study by Bell, Mieth, Röer, Troche, and
  Buchner (2019). Participants were presented with a sequence of digits and were asked to 
  recall them later on in a forward serial order. Participants wore headphones and were asked
  to ignore what they heard. A female voice either repeated the same word 10 times 
  (steady-state condition), disrupted the steady-state sequence with a deviant word in the 
  seventh position of the sequence (auditory deviant condition), or uttered ten different 
  words (changing-state condition).
  The data show that all conditions differ and are therefore evidence both for the 
  changing-state and the auditory deviant effect.
}
\usage{data("bell19")}
\format{
  A data frame with 104832 observations on the following 6 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session number (1 and 2)}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Condition: \code{steady}, \code{auditory deviant}, or \code{changing}}
    \item{\code{serpos}}{Serial position (1-8)}
    \item{\code{correct}}{Correct (1/0)}
  }
}
\details{
  For further details, see Bell et al. (2019).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  Bell, R., Mieth, L., Röer, J. P., Troche, S., & Buchner, A. (2019). Preregistered 
  replication of the auditory deviant effect: A robust benchmark finding. \emph{Journal of 
  Cognition, 2}(1), 13. https://doi.org/10.5334/joc.64
}

\examples{
## Approximate reproduction of Figure 1 in Bell et al. (2019)
data(bell19)
pd <- aggregate(correct ~ serpos+condition, data = bell19, FUN = mean)
plot(c(1,8), c(0.3,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Auditory Deviant Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "steady"], y = pd$correct[pd$condition == "steady"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$condition == "auditory deviant"], 
      y = pd$correct[pd$condition == "auditory deviant"], 
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$condition == "changing"], y = pd$correct[pd$condition == "changing"], 
      type = "b", lty = 3, pch = 18, col = "green")
legend(1, 0.3, c("steady", "auditory deviant", "changing"), lty = 1:3, pch=c(15,17, 18), 
       title = "Condition:",
       col = c("blue", "red", "green"), horiz = F, cex = 0.6, yjust = 0, xjust = 0)
}
\keyword{datasets}
