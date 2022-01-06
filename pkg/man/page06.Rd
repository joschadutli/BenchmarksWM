\name{page06}
\alias{page06}
\docType{data}
\title{
  Data from Experiment 1 in Page et al. (2006) on the Hebb Repitition Effect
}
\description{
  These are the individual-trial data from Experiment 1 in Page, Cumming, Norris, Hitch, 
  and McNeil (2006). Participants were presented with two types of lists of consonants. Hebb
  lists were repeated throughout the experimental session. Filler lists consisted of new
  compositions of consonants in each trial. Additionally, participants performed one 
  experimental block with concurrent articulation (i.e. articulatory suppression; AS) and one
  in silence. Block order was counter-balanced between participants. Within each block,
  every third list was a Hebb repitition list. In the AS block, lists consisted of eight
  letters. In the no-AS block, five letters were presented. This difference was established 
  in pilot work and ensured equal baseline performance in the two conditions.
  Memory was tested in a serial reproduction task, in which all presented letters reappeared
  on the screen and participants had to select the letters in their correct list positions.
  The data show that serial reproduction of Hebb repitition lists improved over experimental
  trials, whereas performance on filler lists doesn't. This pattern was found in both AS
  conditions.
}
\usage{data("page06")}
\format{
  A data frame with 6912 observations on the following 5 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID (1-16 for each list type and AS condition -> 48 trials per block)}
    \item{\code{condition}}{List type condition: \code{FillA}, \code{FillB}, and \code{HebbEffect} (see Details)}
    \item{\code{AS}}{Articulatory suppression: \code{ns} = "no suppression", \code{su} = "suppression"}
    \item{\code{acc}}{Accuracy (proportion correct)}
  }
}
\details{
  To produce Figure 1 in Page et al. (2006) or Figure 21 in Oberauer et al. (2018),
  \code{FillA} lists were excluded. For more details, see Page et al. (2006).
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
  
  Page, M. P. A., Cumming, N., Norris, D., Hitch, G. J., & McNeil, A. M. (2006). Repetition
  learning in the immediate serial recall of visual and auditory materials. \emph{Journal 
  of Experimental Psychology: Learning, Memory, and Cognition, 32}, 716-733. 
  http://dx.doi.org/10.1037/0278-7393.32.4.716
  
}
\examples{
## Produce combined version of Figure 21 in Oberauer et al. (2018)
## (top panel in blue; bottom panel in red)
data(page06)
pd <- aggregate(acc ~ condition + AS + trial, data=page06, FUN=mean)
plot(c(1,16), c(0.3,0.8), type = "n", xlab = "Trial",
     ylab = "Proportion correct", main = "Hebb Repitition Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 
     cex.axis = 0.7)
lines(x = pd$trial[pd$condition == "FillB" & pd$AS == "ns"], 
      y = pd$acc[pd$condition == "FillB" & pd$AS == "ns"], 
      type = "b", lty = 2, pch = 15, col = "blue")
lines(x = pd$trial[pd$condition == "HebbEffect" & pd$AS == "ns"], 
      y = pd$acc[pd$condition == "HebbEffect" & pd$AS == "ns"], 
      type = "b", lty = 2, pch = 16, col = "blue")
lines(x = pd$trial[pd$condition == "FillB" & pd$AS == "su"], 
      y = pd$acc[pd$condition == "FillB" & pd$AS == "su"], 
      type = "b", lty = 1, pch = 17, col = "red")
lines(x = pd$trial[pd$condition == "HebbEffect" & pd$AS == "su"], 
      y = pd$acc[pd$condition == "HebbEffect" & pd$AS == "su"], 
      type = "b", lty = 1, pch = 18, col = "red")
legend(16, 0.3, c("Filler", "Repeating", "Filler - AS", "Repeating - AS"), lty = c(2,1,2,1),
       pch=15:18, col = c("blue", "blue", "red", "red"),
       horiz = F, cex = 0.6, yjust = 0, xjust = 1)
}
\keyword{datasets}
