\name{frankish89}
\alias{frankish89}
\docType{data}
\title{
  Frankish's (1989) Data on the Effect of Grouping in Serial Recall
}
\description{
  Individual-trial data from Experiment 1 in Frankish (1989). Lists of stimuli were divided
  into groups of stimuli by inserting pauses in between certain list items. One experimental 
  group performed the experiment in an auditory setting whereas the other group performed 
  the experiment in a visual setting. Separating parts of a list (i.e. grouping) of stimuli
  increases overall recall performance.
}
\usage{data("frankish89")}
\format{
  A data frame with 17280 observations on the following 10 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{group}}{Experimental group: \code{auditory} or \code{visual} setting}
    \item{\code{condition}}{Condition: \code{grouped} vs. \code{ungrouped}}
    \item{\code{cond_num}}{Exact condition number for plotting concerned with the length of
    the \code{interval} between grouped stimuli (1 = ungrouped)}
    \item{\code{interval}}{Interval between grouped stimuli (0 s for \code{ungrouped})}
    \item{\code{serpos}}{Serial position}
    \item{\code{item}}{Presented item}
    \item{\code{resp}}{Response}
    \item{\code{acc}}{Accuracy: 1 = correct, 0 = incorrect}
  }
}
\details{
  For further details, see Frankish (1989).
}
\source{
    Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Frankish, C. (1989). Perceptual organization and precategorical acoustic storage. 
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 15}, 469 – 479.
  http://dx.doi.org/10.1037/0278-7393.15.3.469
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 18b in Oberauer et al. (2018)
data(frankish89)
subdat <- subset(frankish89, group == "visual")
pd <- aggregate(acc ~ serpos+cond_num, data = subdat, FUN = mean)
bgk <- c("gray","black","gray","black","white")
plot(0,0, xlim=c(0.5,9.5),ylim=c(0.2,1), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1.4,cex.axis=1.2)
title("Visual")
axis(1, at=c(1:9),lwd=1, lwd.ticks=1,cex.axis=1.0)
axis(2, at=seq(from=.3,to=1.,by=.1),lwd=1, lwd.ticks=1,cex.axis=1.0)
for (k in c(1:5)) {
  tbp <- pd[which(pd$cond_num == k),]
  lines(tbp$serpos[1:3],tbp$acc[1:3],lwd=1,lty=k)
  lines(tbp$serpos[4:6],tbp$acc[4:6],lwd=1,lty=k)
  lines(tbp$serpos[7:9],tbp$acc[7:9],lwd=1,lty=k)
  points(tbp$serpos,tbp$acc,pch=20+k,bg=bgk[k],cex=1.0)
}

legend(1,.5,c("Ungrouped","0.25 s", "0.5 s", "1 s", "2 s"),lty=c(1:5),pch=20+c(1:5),pt.bg=bgk,cex=0.6,pt.cex=1.0)

}
\keyword{datasets}
