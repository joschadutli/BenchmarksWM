\name{grenfell12}
\alias{grenfell12}
\docType{data}
\title{
  Data from Grenfell-Essam and Ward's (2012) Experiment 1 on the Probability of 
  First Recall by Serial Position in Free Recall
}
\description{
  Grenfell-Essam and Ward's (2012) Experiment 3 data show that for short lists,
  participants usually start recall with the first list item whereas when the 
  lists get longer, they start with one of the last 4 items in the list. The latter
  effect is stronger, when participants are told before list presentation, that
  they are free to recall the list in any order.
}
\usage{data("grenfell12")}
\format{
  A data frame with 64 observations on the following 4 variables.
  \describe{
    \item{\code{cue}}{Cue for free recall or serial recall: \code{pre-presentation}
    or \code{post-presentation}.}
    \item{\code{output}}{First-item recalled: \itemize{
    \item{\code{Start}}{Starting from the first list item.}
    \item{\code{Last 4}}{Starting with any item from the last 4 list items.}
    \item{\code{Other}}{Starting from somewhere else.}
    \item{\code{Error}}{Starting with an error, i.e. an item that was not part of the lsit.}
    }}
    \item{\code{listlength}}{List length: \code{2}, \code{4}, \code{5}, \code{6}, \code{7}, \code{8}, \code{12}, \code{15}}
    \item{\code{prob_fr}}{Probability of first recall (probability that the first item recalled was in this \code{output} category).}
  }
}
\details{
  For details, see Grenfell-Essam and Ward (2012) or \code{Examples}
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}
\references{
  Grenfell-Essam, R., & Ward, G. (2012). Examining the relationship between free
  recall and immediate serial recall: The role of list length, strategy use,
  and test expectancy. \emph{Journal of Memory and Language, 67}, 106-148. 
  http://dx.doi.org/10.1016/j.jml.2012.04.004
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(grenfell12)
## Reproduce Figure 14 (A&B) in Grnefell-Essam and Ward (2012) or
## Figure 9 in Oberauer et al. (2018), respectively.
post <- filter(grenfell12, cue=="post-presentation")
pre <- filter(grenfell12, cue=="pre-presentation")

colors <- c("black", "grey50", "grey80", "white")

## written by K. Oberauer
plotpanel <- function(post,title4panel) {
  plot(0,0, xlim=c(0,max(post$listlength)+1),ylim=c(0,1), type="n", las=1, xaxt="n",
       xlab="List Length", ylab="Probability of First Recall",cex.lab=0.8)
  k<-0
  for (rx in unique(post$output)) {
    tbp<-filter(post, output==rx)
    k<-k+1
    lines(tbp$listlength, tbp$prob_fr,lwd=1,lty=k)  
    points(tbp$listlength, tbp$prob_fr,pch=20+k, bg=colors[k],cex=0.8)
  }
  text(7.5,.95,title4panel,cex=1.0)
  legend(10,.6,unique(post$output),lty=c(1:k),pch=c(21:(20+k)),pt.bg=colors,pt.cex=0.6,y.intersp = 1, cex = 0.6)
  par(tcl= -0.2)  #minor ticks
  axis(1, at=seq(from=0,to=max(post$listlength)+1,by=1), labels=F, lwd=1, lwd.ticks=0.5)
  par(tcl= -0.5)  #major ticks with labels
  axis(1, at=seq(from=0,to=max(post$listlength)+1,by=3), labels=seq(from=0,to=max(post$listlength)+1,by=3),
       lwd=0, lwd.ticks=1)
}
par(mfrow=c(1,2),mar=c(4, 4, 2, 1) + 0.2)
plotpanel(pre,"Pre-cued")
plotpanel(post,"Post-cued")
}
\keyword{datasets}
