\name{bhatarah09a}
\alias{bhatarah09a}
\docType{data}
\title{
  Data from Experiment 1 in Bhatarah, Ward, Smith, & Hayes (2009) on the Differences Between
  Immediate Serial Recall and Free Recall of Words
}
\description{
  These are the individual-trial data from Experiment 1 in Bhatarah et al. (2009). Free
  recall and immediate serial recall were either announced before presentation of the word
  list (precued) or after the presentation of the list (postcued).
}
\usage{data("bhatarah09a")}
\format{
  A data frame with 6144 observations on the following 10 variables.
  \describe{
    \item{\code{exp}}{Experiment 1}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Task: Free recall (\code{FR}) or immediate serial recall (\code{SR})}
    \item{\code{cue}}{Cue: \code{precued} or \code{postcued}}
    \item{\code{serpos}}{Serial Position (1-8)}
    \item{\code{words}}{Presented words}
    \item{\code{output}}{Output position (\code{NA} for ommissions or extra-list intrusions)}
    \item{\code{recalled}}{Correctly recalled (1/0)}
    \item{\code{spos.correct}}{Recalled at correct serial position (1/0)}
  }
}
\details{
  For further details, see Bhatarah et al. (2009).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  
  Bhatarah, P., Ward, G., Smith, J., & Hayes, L. (2009). Examining the relationship between
  free recall and immediate serial recall: Similar patterns of rehearsal and similar effects
  of word length, presentation rate, and articulatory suppression. \emph{Memory & Cognition,
  37}, 689 –713. http://dx.doi.org/10.3758/MC.37.5.689
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
### Reproduce Figures 1A & 1B in Bhatarah et al. (2009)
data(bhatarah09a)
### Free recall
pd <- aggregate(recalled ~ serpos+cue, 
                data = bhatarah09a[which(bhatarah09a$task == "FR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Free Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$cue == "precued"], y = pd$recalled[pd$cue == "precued"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$cue == "postcued"], 
      y = pd$recalled[pd$cue == "postcued"], 
      type = "b", lty = 2, pch = 17, col = "red")
legend(1, 0.0, c("precued", "postcued"), lty = 1:2, pch=c(15,17), 
       title = "Cue condition:",
       col = c("blue", "red"), horiz = F, cex = 0.6, yjust = 0, xjust = 0)
### Serial recall
pd <- aggregate(spos.correct ~ serpos+cue, 
                data = bhatarah09a[which(bhatarah09a$task == "SR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Immediate Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$cue == "precued"], y = pd$spos.correct[pd$cue == "precued"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$cue == "postcued"], 
      y = pd$spos.correct[pd$cue == "postcued"], 
      type = "b", lty = 2, pch = 17, col = "red")
legend(8, 1.0, c("precued", "postcued"), lty = 1:2, pch=c(15,17), 
       title = "Cue condition:",
       col = c("blue", "red"), horiz = F, cex = 0.6, yjust = 1, xjust = 1)

}
\keyword{datasets}
