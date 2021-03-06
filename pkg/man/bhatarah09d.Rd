\name{bhatarah09d}
\alias{bhatarah09d}
\docType{data}
\title{
  Data from Experiment 4 in Bhatarah, Ward, Smith, & Hayes (2009) on the the Word Length
  Effect with Smaller List Lengths
}
\description{
  These are the individual-trial data from Experiment 4 in Bhatarah et al. (2009). 
  Participants preformed free recall or immediate serial recall of lists of words 
  by experimental block. Word length varied betwee short (one syllable), medium (three
  syllables), and long (five syllables). In contrast to the other experiments, the list 
  length was only six instead of eight.
}
\usage{data("bhatarah09d")}
\format{
  A data frame with 4320 observations on the following 11 variables.
  \describe{
    \item{\code{exp}}{Experiment 4}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Task: Free recall (\code{FR}) or immediate serial recall (\code{ISR})}
    \item{\code{listlength}}{Listlength: constant at 6}
    \item{\code{word.length}}{Word length: \code{short}: one syllable, \code{long}: five syllables}
    \item{\code{serpos}}{Serial Position (1-6)}
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
  37}, 689-713. http://dx.doi.org/10.3758/MC.37.5.689
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduction of Figure 7B in Bhatarah et al. (2009)
data(bhatarah09d)
### Immediate serial recall
pd <- aggregate(spos.correct ~ serpos+word.length, 
                data = bhatarah09d[which(bhatarah09d$task == "ISR"),], FUN = mean)
plot(c(1,6), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Immediate Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short"], 
      y = pd$spos.correct[pd$word.length == "short"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium"], 
      y = pd$spos.correct[pd$word.length == "medium"], 
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long"], 
      y = pd$spos.correct[pd$word.length == "long"], 
      type = "b", lty = 3, pch = 18, col = "green")
legend(6, 1.0, c("short","medium","long"), lty = 1:3, 
       pch=c(15,17,18), title = "Cue condition:", col = c("blue", "red","green"), 
       horiz = FALSE, cex = 0.6, yjust = 1, xjust = 1)
}
\keyword{datasets}
