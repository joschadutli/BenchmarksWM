\name{towse08}
\alias{towse08}
\docType{data}
\title{
  Towse, Cowan, Hitch, and Horton's (2008) effects of list length on response times in a complex span task
}
\description{
  Individual-trial data from Towse, Cowan, Hitch, and Horton's (2008) Experiment
  2 on a complex span task with different list lengths. The preparatory
  interval (time interval before first response) increases with list length.
  The complex span task consists of reading sentences into which a target word
  is integrated or from which the target word is independent.
}
\usage{data("towse08")}
\format{
  A data frame with 168 observations on the following 11 variables.
  \describe{
    \item{\code{subj}}{Unique ID number for each subject}
    \item{\code{set}}{Two sets of sentences: \code{A} and \code{B}}
    \item{\code{condition}}{The target word is either \code{Independent} from the sentence or naturally \code{Integrated} in its meaning.}
    \item{\code{trial}}{Trial number}
    \item{\code{listlength}}{List length: \code{2} or \code{3}}
    \item{\code{read.time}}{The mean time for reading the sentence (including the target word)}
    \item{\code{prep.int}}{The time interval between presentation and onset of the response (preparatory interval)}
    \item{\code{recall.word}}{The mean time needed to reproduce the target words}
    \item{\code{pause}}{The mean time interval between two responses}
    \item{\code{response}}{\code{Correct} or \code{Incorrect}}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the individual-trial data from Experiment 2 of Towse, Cowan,
  Hitch, and Horton (2008), showing an increase in time used to prepare before
  giving a response. The data was listed as a benchmark finding in working memory
  research by Oberauer et al. (2018).

  Two groups completed a complex span task with list lengths 2 or 3. In the
  first group, a sentence had to be read and a target word that could naturally
  be integrated into the meaning of the sentence appeared in purple as the last
  word of the sentence. For the second group, the target word was independent
  of the sentence.

  The preparatory interval is higher for list length 3 in comparison to a
  list length of 2.
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

  Towse, J. N., Cowan, N., Hitch, G. J., & Horton, N. J. (2008). The recall of
  information from working memory. Insights from behavioural and chronometric
  perspectives. \emph{Experimental Psychology, 55}, 371-383.
  http://dx.doi.org/10.1027/1618-3169.55.6.371
}
\examples{
data(towse08)

## Reconstructing Figure 2 of Towse et al. (2008)

data("towse08")
plotp <- aggregate(prep.int ~ condition*listlength, data = towse08, FUN = mean)
plotw <- aggregate(recall.word ~ condition*listlength, data = towse08, FUN = mean)
plotpause <- aggregate(pause ~ condition*listlength, data = towse08, FUN = mean)
# reorder data for plot
plotp <- plotp[c(2,1,4,3),]
plotw <- plotw[c(2,1,4,3),]
plotpause <- plotpause[c(2,1,4,3),]
### recreate barplot in Towse et al. (2008)
oldpar <- par(no.readonly = TRUE)

par(mfrow=c(1,3))
barplot(as.table(plotp$prep.int), main="Recall segments",
        xlab="Preparatory Interval", col=c("white", "lightgrey", "darkgrey", "black"),
        beside=TRUE, ylim = c(0,2))
barplot(as.table(plotw$recall.word),
        xlab="Words", col=c("white", "lightgrey", "darkgrey", "black"),
        ylim = c(0,2))
barplot(as.table(plotpause$pause),
        xlab="Pause", col = c("white", "lightgrey", "darkgrey", "black"),
        beside=TRUE, ylim = c(0,2))
legend(0.1,2, legend = c("LL 2 integrated", "LL 2 independent",
           "LL 3 integrated", "LL 2 independent", "LL = list length"),
       col=c("white", "lightgrey", "darkgrey", "black"),
       fill=c("white", "lightgrey", "darkgrey", "black", NA))
par(oldpar)
}
\keyword{datasets}
