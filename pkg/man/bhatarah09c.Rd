\name{bhatarah09c}
\alias{bhatarah09c}
\docType{data}
\title{
  Data from Experiment 3 in Bhatarah, Ward, Smith, & Hayes (2009) on the the Word Length
  Effect and the Effects of Articulatory Suppression
}
\description{
  These are the individual-trial data from Experiment 3 in Bhatarah et al. (2009). 
  Participants preformed free recall or immediate serial recall of lists of words 
  by experimental block. Word length varied betwee short (one syllable) and long 
  (five syllables). Partcipants were asked to repeat aloud the word "the" during 
  presentation of the word lists (articulatory suppression) or to keep quiet (quiet).
}
\usage{data("bhatarah09c")}
\format{
  A data frame with 9216 observations on the following 10 variables.
  \describe{
    \item{\code{exp}}{Experiment 3}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{block}}{Block ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Task: Free recall (\code{FR}) or immediate serial recall (\code{ISR})}
    \item{\code{activity}}{Activity: \code{articulatory suppression} or \code{quiet}}
    \item{\code{word.length}}{Word length: \code{short}: one syllable, \code{long}: five syllables}
    \item{\code{serpos}}{Serial Position (1-8)}
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
### Reproduce Figure 6B in Bhatarah et al. (2009)
data(bhatarah09c)
### Immediate serial recall
pd <- aggregate(spos.correct ~ serpos+activity+word.length, 
                data = bhatarah09c[which(bhatarah09c$task == "ISR"),], FUN = mean)
plot(c(1,8), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Immediate Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8), labels = levels(pd$serpos), cex.axis = 0.7)
lines(x = pd$serpos[pd$word.length == "short" & pd$activity == "articulatory suppression"], 
      y = pd$spos.correct[pd$word.length == "short" & 
                            pd$activity == "articulatory suppression"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "long" & pd$activity == "articulatory suppression"], 
      y = pd$spos.correct[pd$word.length == "long" 
                          & pd$activity == "articulatory suppression"], 
      type = "b", lty = 1, pch = 17, col = "green")
lines(x = pd$serpos[pd$word.length == "short" & pd$activity == "quiet"], 
      y = pd$spos.correct[pd$word.length == "short" & pd$activity == "quiet"], 
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$serpos[pd$word.length == "long" & pd$activity == "quiet"], 
      y = pd$spos.correct[pd$word.length == "long" & pd$activity == "quiet"], 
      type = "b", lty = 2, pch = 17, col = "green")
legend(8, 1.0, c("AS-short","AS-long",
                 "quiet-short","quiet-long"), lty = c(1,1,2,2), 
       pch=c(15,17,15,17), title = "Cue condition:",
       col = c("blue","green","blue","green"), horiz = F, cex = 0.6, 
       yjust = 1, xjust = 1)
}
\keyword{datasets}
