\name{jarrold10}
\alias{jarrold10}
\docType{data}
\title{
  Jarrold, Tam, Baddeley, and Harvey's (2010) Data on the Effects of Type and Position
  of Processing Tasks on Serial Recall in Working Memory
}
\description{
  Individual-trial data from Jarrold et al. (2010). Six words were presented in sequence
  in each trial. Between two of the six words, participants had to perform a processing
  task. The experimental conditions manipulated the serial position of the processing task
  within trials. Type of processing task was manipulated between groups. One group performed
  a spatial processing task (symmetry judgment) and the other performed a verbal task 
  (rhyme judgment). 
  
  The data show an effect of the type of processing task. Processing in the same domain as 
  the stimulus material (i.e. verbal processing) resulted in poorer recall performance
  than visual processing. Moreover, the processing tasks mainly affected memory for items
  preceeding the processing task.
}
\usage{data("jarrold10")}
\format{
  A data frame with 17640 observations on the following 11 variables.
  \describe{
    \item{\code{id}}{Unique subject IDs for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{group}}{Processing Group: \code{symmetry} or \code{rhyme}}
    \item{\code{cond}}{Numbered Conditions (for plotting): \code{1} = \code{SixZero}, \code{2} = \code{FiveOne}, etc.}
    \item{\code{condition}}{Conditions: e.g.\code{FiveOne} = 5 words -> processing task -> 1 word 
    The factor levels are: \code{SixZero} < \code{FiveOne} < \code{FourTwo} < \code{ThreeThree} < \code{TwoFour} < \code{OneFive} < \code{ZeroSix}}
    \item{\code{serpos}}{Serial Position (1-6)}
    \item{\code{item}}{Presented word}
    \item{\code{response}}{Typed response}
    \item{\code{correct}}{Correct serial recall (1/0)}
    \item{\code{recalled}}{Recalled regardless of output position (1/0)}
    \item{\code{transposition}}{Transposition errors (e.g. first item at second output position). \code{1} = transposition error, \code{0} = correct output/omission/intrusion}
  }
}
\details{
  The condition factor is ordered in reverse. That is, the first condition in the data
  is defined by a processing task at the end of the presentation of the six words (this
  was the hardest condition due to the data). 
  For further details, see Jarrold et al. (2010).
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Jarrold, C., Tam, H., Baddeley, A. D., & Harvey, C. E. (2010). The nature and position 
  of processing determines why forgetting occurs in working memory tasks. \emph{Psychonomic
  Bulletin & Review, 17}, 772–777. http://dx.doi.org/10.3758/PBR.17.6.772
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Produce an overall Figure
data(jarrold10)
pd <- aggregate(correct ~ cond+condition+group, data = jar, FUN = mean)
plot(c(1,7), c(0.0,1.0), type = "n", xlab = "Condition",
     ylab = "Proportion correct", main = "Positions of Processing Interval 
     in Serial Recall", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7), labels = levels(pd$condition), cex.axis = 0.7)
lines(x = pd$cond[pd$group == "rhyme"], y = pd$correct[pd$group == "rhyme"], 
      type = "b", lty = 1, pch = 15, col = "red")
lines(x = pd$cond[pd$group == "symmetry"], y = pd$correct[pd$group == "symmetry"], 
      type = "b", lty = 2, pch = 17, col = "blue")
legend(7, 0, c("Rhyme", "Symmetry"), lty = 1:2, pch=c(15,17), title = "Processing Task:",
       col = c("red", "blue"), horiz = F, cex = 0.6, yjust = 0, xjust = 1)
}
\keyword{datasets}
