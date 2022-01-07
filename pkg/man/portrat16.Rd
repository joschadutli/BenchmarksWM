\name{portrat16}
\alias{portrat16}
\docType{data}
\encoding{UTF-8}
\title{
  Portrat, Guida, Phénix, & Lemaire's (2016) Data on Chunking
}
\description{
  These are the individual-subject data from Portrat, Guida, Phénix, and Lemaire (2016).
  Well-known acronyms were presented in different serial positions in a complex span task 
  with letters as memory stimuli, or not at all. The data show that serial recall of letters
  improves when the lists contain known acronyms, especially in early list positions.
}
\usage{data("portrat16")}
\format{
  A data frame with 256 observations on the following 4 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID}
    \item{\code{condition}}{Condition (acronym position) \itemize{
    \item{\code{0}}{no acronym in list} 
    \item{\code{1}}{acronym starts at first list position} 
    \item{\code{3}}{acronym starts at third list position} 
    \item{\code{5}}{acronym starts at fifth list position (is at the end of the list)} }}
    \item{\code{pace}}{Presentation pace of distractor task: \code{fast} vs. \code{slow}}
    \item{\code{acc}}{Mean number of correctly recalled items}
  }
}
\details{
  For further detail, see Portrat, Guida, Phénix, & Lemaire (2016).
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
  
  Portrat, S., Guida, A., Phénix, T., & Lemaire, B. (2016). Promoting the experimental
  dialogue between working memory and chunking: Behavioral data and simulation. 
  \emph{Memory & Cognition, 44}, 420-434. http://dx.doi.org/10.3758/s13421-015-0572-9
  
}
\examples{
## Reproduce Figure 20 in Oberauer et al. (2018)
data(portrat16)
pd <- aggregate(acc ~ pace+condition, data = portrat16, FUN = mean)
spacing <- c(0.2,0.2,0.7,0.2,0.7,0.2,0.7,0.2)
bp = barplot(pd$acc, col=c("darkgrey", "lightgrey", "darkgrey", "lightgrey", 
                           "darkgrey", "lightgrey"), space = spacing,
             ylab="Number of Items Recalled", xlab="Acronym Condition", 
             axisnames=F, ylim = c(0,7))

box()
axis(1, at = c(1.3,4.2,7.1,10), labels=c(0,1,3,5), cex.axis=0.7)
legend(9.5, 6.5, c("fast", "slow"), fill=c("darkgrey", "lightgrey"),
       horiz = F, cex = 0.6, yjust = 1, xjust = 0)
}
\keyword{datasets}
