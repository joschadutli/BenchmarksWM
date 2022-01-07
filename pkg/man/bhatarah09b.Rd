\name{bhatarah09b}
\alias{bhatarah09b}
\docType{data}
\title{
  Data from Experiment 2 in Bhatarah, Ward, Smith, & Hayes (2009) on the the Word Length
  Effect and the Effects of Presentation Rate
}
\description{
  These are the individual-trial data from Experiment 2 in Bhatarah et al. (2009). 
  Participants preformed free recall or immediate serial recall of lists of words 
  by experimental block. Word length varied betwee short (one syllable), medium (three
  syllables), and long (five syllables). Presentation rate varied between fast (1s / word)
  and slow (2.5 s / word).
  
}
\usage{data("bhatarah09b")}
\format{
  A data frame with 13824 observations on the following 13 variables.
  \describe{
    \item{\code{exp}}{Experiment 2}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{block}}{Block ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Task: Free recall (\code{FR}) or immediate serial recall (\code{ISR})}
    \item{\code{presrate}}{Presentation rate: \code{fast} vs. \code{slow}}
    \item{\code{word.length}}{Word length: \code{short}: one syllable, \code{medium}: three syllables, \code{long}: five syllables}
    \item{\code{serpos}}{Serial Position (1-8)}
    \item{\code{rehearsals}}{Number of rehearsals at this serial input position}
    \item{\code{last.rehearsal}}{Last rehearsal}
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
  memory. \emph{Psychological Bulletin, 144}, 885-958.}
\examples{
### Approximate reproduction of Figure 4 (top panel) from Bhatarah et al. (2009)
data(bhatarah09b)
### Free recall
pd <- aggregate(recalled ~ serpos+presrate+word.length, 
                data = bhatarah09b[which(bhatarah09b$task == "FR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Free Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short" & pd$presrate == "slow"], 
      y = pd$recalled[pd$word.length == "short" & pd$presrate == "slow"], 
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium" & pd$presrate == "slow"], 
      y = pd$recalled[pd$word.length == "medium" & pd$presrate == "slow"], 
      type = "b", lty = 1, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long" & pd$presrate == "slow"], 
      y = pd$recalled[pd$word.length == "long" & pd$presrate == "slow"], 
      type = "b", lty = 1, pch = 18, col = "green")
lines(x = pd$serpos[pd$word.length == "short" & pd$presrate == "fast"], 
      y = pd$recalled[pd$word.length == "short" & pd$presrate == "fast"], 
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "medium" & pd$presrate == "fast"], 
      y = pd$recalled[pd$word.length == "medium" & pd$presrate == "fast"], 
      type = "b", lty = 2, pch = 17, col = "red")
lines(x = pd$serpos[pd$word.length == "long" & pd$presrate == "fast"], 
      y = pd$recalled[pd$word.length == "long" & pd$presrate == "fast"], 
      type = "b", lty = 2, pch = 18, col = "green")
legend(8, 0.0, c("slow-short","slow-medium","slow-long",
                 "fast-short","fast-medium","fast-long"), lty = c(1,1,1,2,2,2), 
       pch=c(15,17,18,15,17,18), title = "Cue condition:",
       col = c("blue", "red","green","blue", "red","green"), horiz = FALSE, cex = 0.6, 
       yjust = 0, xjust = 1)

}
\keyword{datasets}
