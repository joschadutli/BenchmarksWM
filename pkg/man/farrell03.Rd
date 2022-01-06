\name{farrell03}
\alias{farrell03}
\docType{data}
\title{
  Farrell and Lewandowsky's (2003) Data on the Effects of Phonological Similarity in Serial
  Recall
}
\description{
  These are the individual-trial data from Farrell and Lewandowsky (2003). In Experiment 1,
  participants were presented with different combinations of phonologically similar and
  dissimilar consonants. Memory was tested with immediate serial recall. Experiment 2 was
  exactly the same, except that memory was tested with a reconstruction task. All memory
  items were present in a random order in the top of the screen and participants had to type
  in the items in the same serial order as they were presented.
}
\usage{data("farrell03")}
\format{
  A data frame with 34200 observations on the following 11 variables.
  \describe{
    \item{\code{exp}}{Experiment: \code{1} or \code{2}}
    \item{\code{subj}}{Unique subject ID for each participant in each experiment}
    \item{\code{trial}}{Trial ID}
    \item{\code{task}}{Memory task: \code{serial recall} or \code{reconstruction}}
    \item{\code{condition}}{Condition (D = dissimilar item; S = similar item: \code{DDDDDD}, \code{SSSSSS}, \code{SDSSSS}, \code{SSSDSS}, \code{SSSSSD}, or \code{SDSDSD}}
    \item{\code{serpos}}{Serial Position}
    \item{\code{similarity}}{Similarity of item: \code{S} = similar item, \code{D} = dissimilar item}
    \item{\code{output}}{Output position (>6 for ommissions; 0 for extra-list instrusions)}
    \item{\code{acc}}{Correct (1/0)}
    \item{\code{rt}}{Response time (ms)}
  }
}
\details{
  The list of phonologically similar consonants is: [B,D,G,P,T,V]. The list of dissimilar
  items is: [H,K,M,Q,R,Y].
  It is unclear whether output positions < 0 are ommissions or extra-list intrusions.
  For further details, see Farrell and Lewandowsky (2003).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Farrell, S., & Lewandowsky, S. (2003). Dissimilar items benefit from phonological 
  similarity in serial recall. \emph{Journal of Experimental Psychology: Learning, Memory, 
  and Cognition, 29}, 838-849. http://dx.doi.org/10.1037/0278-7393.29.5.838
  
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 16 (upper panel) in Oberauer et al. (2018)
data(farrell03)
psd <- farrell03[which(farrell03$exp == 1),]
pd <- aggregate(acc ~ serpos+condition, data = psd, FUN = mean)
plot(c(1,6), c(0.0,1.0), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct", main = "Phonological Similarity", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = levels(pd$serpos), 
     cex.axis = 0.7)
lines(x = pd$serpos[pd$condition == "DDDDDD"], 
      y = pd$acc[pd$condition == "DDDDDD"], 
      type = "b", lty = 1, pch = 15, col = "black")
lines(x = pd$serpos[pd$condition == "SSSSSS"], 
      y = pd$acc[pd$condition == "SSSSSS"], 
      type = "b", lty = 2, pch = 17, col = "grey")
legend(1, 0.0, c("dissimilar","similar"), lty = 1:2, 
       pch=c(15,17), col = c("black", "grey"), 
       horiz = F, cex = 0.6, yjust = 0, xjust = 0)
}
\keyword{datasets}
