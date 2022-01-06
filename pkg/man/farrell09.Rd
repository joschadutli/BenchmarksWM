\name{farrell09}
\alias{farrell09}
\docType{data}
\title{
  Farrell & Lelièvre's (2009) Data on the Effects of Grouping
}
\description{
  Individual-trial data from Experiments 1 & 2 in Farrell and Lelièvre (2009). In Experiment
  1, lists were ungrouped in one session and temporally divided into two groups of four items
  in the second session. In Experiment 2, each list consisted either of two groups of four, or
  a group of three followed by a group of four items, or vice-versa. 
}
\usage{data("farrell09")}
\format{
  A data frame with 59200 observations on the following 10 variables.
  \describe{
    \item{\code{exp}}{Experiment (1 or 2)}
    \item{\code{subj}}{Unique subject ID within experiments}
    \item{\code{trial}}{Trial ID}
    \item{\code{grouping}}{Grouping: \code{4-4}, \code{4-3}, or \code{3-4}}
    \item{\code{dist.pc}}{Proportion correct on parity task between list presentation and recall in Experiment 2 (ms; NA in Exp 1)}
    \item{\code{dist.rt}}{Mean response time on parity task}
    \item{\code{serpos}}{Serial Position}
    \item{\code{outpos}}{Output}
    \item{\code{acc}}{Accuracy (1 = correct, 0 = incorrect)}
    \item{\code{rt}}{Response time (ms)}
  }
}
\details{
  For further details, see Farrell and Lelièvre (2009).
}
\source{
      Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Farrell, S., & Lelièvre, A. (2009). End anchoring in short-term order memory. \emph{
  Journal of Memory and Language, 60}, 209-227. http://dx.doi.org/10.1016/j.jml.2008.09.004
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 8 in Farrell & Lelièvre (2009)
data(farrell09)
subdat <- subset(farrell09, exp == 2)
pd <- aggregate(acc ~ grouping+serpos, data = subdat, FUN = mean)
bgk <- c("black","gray","white")
plot(0,0, xlim=c(0.5,8.5),ylim=c(0.4,0.9), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1.4,cex.axis=1.2)
title("Effects of Grouping")
axis(1, at=c(1:8),lwd=1, lwd.ticks=1,cex.axis=1.0)
axis(2, at=seq(from=.4,to=.9,by=.1),lwd=1, lwd.ticks=1,cex.axis=1.0)
ff <- subset(pd, grouping == '4-4')
lines(ff$serpos[1:4],ff$acc[1:4],lwd=1,lty=1)
lines(ff$serpos[5:8],ff$acc[5:8],lwd=1,lty=1)
points(ff$serpos,ff$acc,pch=20+1,bg=bgk[1],cex=1.0)
tf <- subset(pd, grouping == '3-4')
lines(tf$serpos[1:3],tf$acc[1:3],lwd=1,lty=1)
lines(tf$serpos[4:7],tf$acc[4:7],lwd=1,lty=1)
points(tf$serpos,tf$acc,pch=20+3,bg=bgk[3],cex=1.0)
ft <- subset(pd, grouping == '4-3')
lines(ft$serpos[1:4],ft$acc[1:4],lwd=1,lty=1)
lines(ft$serpos[5:7],ft$acc[5:7],lwd=1,lty=1)
points(ft$serpos,ft$acc,pch=20+2,bg=bgk[2],cex=1.0)
legend(1,.55,c("4-4","4-3", "3-4"),pch=20+c(1:3),pt.bg=bgk,cex=0.8,pt.cex=1.0)
}
\keyword{datasets}
