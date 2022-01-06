\name{jarrold13}
\alias{jarrold13}
\docType{data}
\title{
  Jarrold & Citroën's (2013) Data on the Phonological Similarity Effect in Children
}
\description{
  These are the individual-subject data from Jarrold and Citroën (2013). Children from
  different age groups were presented with similar and dissimilar lists of words that
  they had to recall later on. Encoding and recall varied between a visual and a verbal
  setting across groups. The data show a consistent effect of phonological similarity
  over task settings and age groups.
}
\usage{data("jarrold13")}
\format{
  A data frame with 116 observations on the following 12 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{grade.level}}{Grade Level: \code{K} < \code{1} < \code{2} < \code{3}}
    \item{\code{age}}{Age}
    \item{\code{digit.span}}{Proportion correct on a pre-screening digit span task}
    \item{\code{order}}{Counterbalanced order of list presentation: D=dissimilar; S=similar \code{D-S} \code{S-D}}
    \item{\code{condition}}{Condition number (combination of \code{encoding} and \code{recall})}
    \item{\code{encoding}}{Encoding setting: \code{visual} or \code{verbal}}
    \item{\code{recall}}{Recall setting: \code{visual} or \code{verbal}}
    \item{\code{dissimilar.pc}}{Partial credit scores on dissimilar list}
    \item{\code{similar.pc}}{Partial credit scors on similar lists}
    \item{\code{absolute.pse}}{Absolute Phonological Similarity Effect: \code{dissimilar.pc}-\code{similar.pc}}
    \item{\code{proportional.pse}}{Proportional Phonological Similarity Effect: (\code{dissimilar.pc}-\code{similar.pc})/\code{dissimilar.pc}}
  }
}
\details{
  The dissimilar word list was: bell, bird, chair, door, fork, kite, shoe, tree, watch.
  The similar word list was: bag, bat, cat, hat, man, pan, rat, tap, van.
  Condition 1 (visual encoding - verbal recall) was the hardest, especially for children in
  kindergarden. For further details, see Jarrold and Citroën (2013).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  
  Jarrold, C., & Citroën, R. (2013). Reevaluating key evidence for the development of
  rehearsal: Phonological similarity effects in children are subject to proportional 
  scaling artifacts. \emph{Developmental Psychology, 49}, 837-847. 
  http://dx.doi.org/10.1037/a0028771
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 16 (lower panel) in Oberauer et al. (2018)
data(jarrold13)
pd <- aggregate(absolute.pse ~ grade.level+condition, data = jarrold13, FUN = mean)

plot(c(0,5), c(-2,8), type = "n", xlab = "Grade Level",
     ylab = "Phonological Similarity", 
     main = "Absolute Phonological Similarity Effect", xaxt = "n")
axis(side = 1, at = c(1,2,3,4), labels = levels(pd$grade.level), 
     cex.axis = 0.7)
lines(x = pd$grade.level[pd$condition == 1], 
      y = pd$absolute.pse[pd$condition == 1], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$grade.level[pd$condition == 2], 
      y = pd$absolute.pse[pd$condition == 2], 
      type = "b", lty = 2, pch = 16, col = "darkgrey")
lines(x = pd$grade.level[pd$condition == 3], 
      y = pd$absolute.pse[pd$condition == 3], 
      type = "b", lty = 3, pch = 17, col = "grey")
lines(x = pd$grade.level[pd$condition == 4], 
      y = pd$absolute.pse[pd$condition == 4], 
      type = "b", lty = 4, pch = 18, col = "lightgrey")
legend(5, -2, c("visual-verbal","visual-visual","verbal-verbal","verbal-visual"), 
       lty = 1:4, 
       pch=15:18, col = c("black", "darkgrey", "grey","lightgrey"), 
       horiz = F, cex = 0.6, yjust = 0, xjust = 1)
}
\keyword{datasets}
