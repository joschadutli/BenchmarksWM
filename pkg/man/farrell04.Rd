\name{farrell04}
\alias{farrell04}
\docType{data}
\title{
  Farrell and Lewandowsky's (2004; Exp. 2) Response Latency and Output Position
  Data for Serial Positions with and without Interference (Distractor Task)
}
\description{
  These are the individual-trial data from Experiment 2 in Farrell
  and Lewandowsky (2004). Stimuli were single consonants. In interference trials,
  participants read four digits aloud before performing forward serial recall.
  No interference trials are characterized by immediate forward serial recall.

  In Farrell and Lewandowsky (2004), output positions were transformed
  to transposition distances of the form: P(Response)/P(Opportunity).
}
\usage{data("farrell04")}
\format{
  A data frame with 12600 observations on the following 5 variables.
  \describe{
    \item{\code{subject}}{Unique subject ID for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Condition: \itemize{\item{\code{0} = No Interference}
    \item{\code{1} = Interference}}}
    \item{\code{serpos}}{Serial Position}
    \item{\code{outpos}}{Output Position (omissions = NA; correct if not NA)}
    \item{\code{rt}}{Response Latency in milliseconds (ms)}
  }
}
\details{
  For details, see Farrell and Lewandowsky (2004) or Figures 10 & B2 in Oberauer et al.
  (2018), which can be partly reproduced using the code in Examples.
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Farrell, S., & Lewandowsky, S. (2004). Modelling transposition latencies:
  Constraints for theories of serial order memory. \emph{Journal of Memory and
  Language, 51}, 115-135. http://dx.doi.org/10.1016/j.jml.2004.03.007

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

}
\examples{
data(farrell04)
### Reproduce Figure B2 in Oberauer et al. (2018) without CIs
fl04m <- aggregate(rt~condition+serpos, data=farrell04, FUN=mean)
#accuracy and then RT
bgk <- c("gray","black")
ltyk <- c("solid","dashed")

# Now plot RT by serial position
plot(c(0,7),c(0,3000), xlim=c(0.5,6.5),ylim=c(0,3100), type="n", las=1,
     xlab="Serial Position", ylab="Mean Latency (ms)",cex.lab=1.2,cex.axis=1.)
for (k in c(0:1)) {
  tbp <- fl04m[which(fl04m$condition == k),]
  xx <- tbp$serpos - 0.05 + k*0.1
  lines(xx,tbp$rt,lwd=2,lty=ltyk[k+1])
  points(xx,tbp$rt,pch=21+k,bg=bgk[k+1],cex=1.5)
}
legend(6,2900,c("No interference","Interference"),lty=ltyk,pch=20+c(1:2),
       pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=1)


}
\keyword{datasets}
