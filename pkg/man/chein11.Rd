\name{chein11}
\alias{chein11}
\docType{data}
\title{
  Chein, Moore, and Conway's (2011) Data on the Effects of Multiple Task Demands on Accuracy
}
\description{
  Individual-subject data from Chein, Moore, and Conway (2011). Participants were asked to 
  memorize either four letters or four locations in a 4x4 grid on the screen. Between items,
  participant either solved a task involving the same or a different stimulus domain, or
  not a distractor task at all. In the spatial distractor task, symmetry had to be judged. 
  The verbal distractor task was a lexical decision task, in which words had to be 
  discriminated from non-words.
  
  The data show that distractor tasks from the same domain as the stimulus material impair
  memory for the stimuli more than does a distractor task from another stimulus domain.
}
\usage{data("chein11")}
\format{
  A data frame with 72 observations on the following 3 variables.
  \describe{
    \item{\code{stimuli}}{Stimulus material: \code{letters}, \code{locations}}
    \item{\code{task}}{Distractor task: \code{lexical}, \code{symmetry}, \code{none}}
    \item{\code{acc}}{Accuracy: Mean number of correctly recalled words per trial.}
  }
}
\details{
 For further details, see Chein et al. (2011).
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Chein, J. M., Moore, A. B., & Conway, A. R. A. (2011). Domain-general mechanisms of
  complex working memory span. \emph{NeuroImage, 54}, 550-559. 
  http://dx.doi.org/10.1016/j.neuroimage.2010.07.067
  
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Approximatly reproduce Figure 13 in Oberauer et al. (2018)
data(chein11)
pd <- aggregate(acc ~ task*stimuli, data = chein11, FUN = mean)
pd <- pd[c(1,2,3,5,4,6),] ## should be lex -> sym for verbal and sym -> let for spatial
barspacing <- c(0.2,0.2,0.2,0.7,0.2,0.2)
legendtext <- c("Lexical", "Symmetry", "None", "Symmetry", "Lexical", "None")
bp = barplot(pd$acc, space = barspacing, col=c(rep("dark gray",3),rep("light gray",3)),
        ylab="Mean Number of Items Recalled", xlab="Distractor Task", axisnames=F, 
        ylim = c(1.9,4.5), xpd = F)
text (2,4.2,"Verbal Memory",cex=1)
text (6,4.2,"Spatial Memory",cex=1)
box()
axis(1, at = bp[,1], labels=legendtext, cex.axis=0.7)
}
\keyword{datasets}
