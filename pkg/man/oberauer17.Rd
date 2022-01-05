\name{oberauer17}
\alias{oberauer17}
\docType{data}
\title{
  Oberauer and Lin's (2017) Data on the Retro-Cue Effect in Continuous Reproduction
  in Visual Working Memory
}
\description{
  These are the individual-trial data from Experiment 3 in Oberauer and Lin (2017). Stimuli
  were memory arrays of colored circles. Retro-cues were either valid, invalid, or neutral.
  At test, participants responded to the target item by selecting a color on a colorwheel.
  The data show an effect of cue condition such that valid cues reduce the size of errors,
  and invalid cues increase the size of errors, relative to neutral cues.
}
\usage{data("oberauer17")}
\format{
  A data frame with 18900 observations on the following 9 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Cue condition: \itemize{\item{\code{neutral}}
    \item{\code{valid}}
    \item{\code{invalid}}
    }}
    \item{\code{setsize}}{Setsize (1,2,4,6,8)}
    \item{\code{target}}{Target color in radians (from the colorwheel)}
    \item{\code{response}}{Response in radians}
    \item{\code{error}}{Error in radians (absolute value)}
    \item{\code{errordeg}}{Error in degrees}
  }
}
\details{
  In the neutral cue condition, an uninformative cue was presented instead of a retro-cue.
  For further details, see Oberauer and Lin (2017).
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
  
  Oberauer, K., & Lin, H. Y. (2017). An interference model of visual working memory. 
  \emph{Psychological Review, 124}(1), 21. http://dx.doi.org/10.1037/rev0000044
  
}
\examples{
## Reproduce Figure 10 (lower panel) in Oberauer and Lin (2017)
pd <- aggregate(errordeg ~ + setsize + condition, data=oberauer17, FUN=mean)
plot(c(1,8), c(0,80), type = "n", xlab = "Set Size",
     ylab = "Error (deg)", main = "Retro-Cue Effect in Continuous Reproduction", xaxt = "n")
axis(side = 1, at = c(2,4,6,8), labels = c(2,4,6,8), 
     cex.axis = 0.7)
lines(x = pd$setsize[pd$condition == "neutral"], 
      y = pd$errordeg[pd$condition == "neutral"], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$setsize[pd$condition == "valid"], 
      y = pd$errordeg[pd$condition == "valid"], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$setsize[pd$condition == "invalid"], 
      y = pd$errordeg[pd$condition == "invalid"], 
      type = "b", lty = 3, pch = 17)
legend(8, 0, c("neutral", "valid", "invalid"), lty = 1:3, 
       pch=15:17, 
       horiz = F, cex = 0.6, yjust = 0, xjust = 1, title = "Cue condition:")

}
\keyword{datasets}
