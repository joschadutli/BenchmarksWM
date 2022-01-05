\name{macnamara11}
\alias{macnamara11}
\docType{data}
\title{
  Macnamara, Moore, & Conway's (2011) Data on the Phonological Similarity Effect in Simple 
  and Complex Span Tasks
}
\description{
  These are the individual-subject data from Experiments 1 & 2 in Macnamara, Moore, and Conway
  (2011). In Experiment 1 partipants were assinged to groups which either had to perform a
  (simple) word span task, a (complex) sentence span task, or a (complex) story span task.
  To-be-remembered words were either simmilar or dissimilar, manipulated over trials within-
  subjects. The data show that in sentence and story span tasks, phonological similarity
  has a facilitating effect on memory performance. In Experiment 2, only the sentence span
  task was used with different sentence lengths manipulated between groups. In addition, 
  target words were uncoupled from the sentences meanings. This resulted in the more typical
  phonological similarity effect where similarity disrupts memory.
}
\usage{data("macnamara11")}
\format{
  A data frame with 404 observations on the following 7 variables.
  \describe{
    \item{\code{exp}}{Experiment (1 or 2)}
    \item{\code{subj}}{Unique subject ID for each participant within experiment}
    \item{\code{task}}{Task: \code{word span}, \code{sentence span}, or \code{story span}}
    \item{\code{length}}{Sentence length (only relevant in Exp 2) \code{short} or \code{long}}
    \item{\code{recall}}{Recall task: \code{serial} or \code{free}}
    \item{\code{list}}{Type of word list: \code{similar} or \code{dissimilar}}
    \item{\code{span}}{Partial credit scored memory span}
  }
}
\details{
  For the definition of memory span and further details, see Macnamara et al. (2011).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  
  Macnamara, B. N., Moore, A. B., & Conway, A. R. A. (2011). Phonological similarity 
  effects in simple and complex span tasks. \emph{Memory & Cognition, 39}, 1174–1186. 
  http://dx.doi.org/10.3758/s13421-011-0100-5
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 2 (top panel) in Macnamara et al. (2011; Exp 1)
data(macnamara11)
psd <- macnamara11[which(macnamara11$exp == 1 & macnamara11$recall == "serial"),]
pd <- aggregate(span ~ task+list, data = psd, FUN = mean)
pd <- pd[c(6,3,4,1,5,2),] 
barspacing <- c(0.2,0.2,0.7,0.2,0.7,0.2)
legendtext <- c("","Words", "Sentences", "Stories","")
bp = barplot(pd$span, space = barspacing, col=c("dark gray","light gray","dark gray",
                                               "light gray","dark gray","light gray"),
             ylab="Serial Span", xlab="Task", axisnames=T, 
             ylim = c(0,1), xpd = T)
axis(1, at = c(0,1.3,4.2,7.1,8.2), labels=legendtext, cex.axis=1, outer = F)
legend(8, 1, c("similar", "dissimilar"), 
       pch=c(15,15), col = c("darkgrey", "lightgrey"), 
       horiz = F, cex = 0.8, yjust = 1, xjust = 1)
}
\keyword{datasets}
