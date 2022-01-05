\name{oberauer06}
\alias{oberauer06}
\docType{data}
\title{
  Oberauer's (2006) Data on Switch Costs and Practice Effects in an N-Back Task
}
\description{
  These are the individual-trial data from Experiment 1 in Oberauer (2006). Depending on 
  set size, digits appeared in several columns on the screen and participants were asked to
  decide whether the currently displayed digit matched the digit last presented in this 
  column. The data show a switch cost such that participants were slower and less accurate
  when consecutive digits were from different columns relative to consecutive digits 
  presented in the same column.
}
\usage{data("oberauer06")}
\format{
  A data frame with 80500 observations on the following 19 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{setsize}}{Set size (1-5; equivalent to number of columns; set size 1 means no switch items.)}
    \item{\code{init.dig1}-\code{init.dig5}}{Initially presented digits. 0 in unused columns}
    \item{\code{serpos}}{Serial Position in n-back task (1-20)}
    \item{\code{digit}}{Presented digit}
    \item{\code{column}}{Column in which digit was presented}
    \item{\code{row}}{Row in which digit was presented}
    \item{\code{update_type}}{Type of updating step: \itemize{ \item{\code{first item}}{ (excluded from analysis)} \item{\code{no-switch}} \item{\code{switch}}}}
    \item{\code{dist}}{Unclear; could be a spatial distance across columns and rows.}
    \item{\code{ptype}}{Probe type: \itemize{ \item{\code{match}} \item{\code{mismatch}} \item{\code{intrusion}}{ (presented digit identical to a digit currently held in WM from another column)}}}
    \item{\code{lag}}{Lag in serial positions since current column was last updated}
    \item{\code{acc}}{Correct/Incorrect response (1/0)}
    \item{\code{rt}}{Response time (ms)}
  }
}
\details{
  The meaning of the variable \code{dist} is undetermined. For further details about the 
  procedure of the experiment, see the Method section and Figure 2 in Oberauer (2006) in
  particular.
}
\source{
      Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Oberauer, K. (2006). Is the focus of attention in working memory expanded through 
  practice? \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 32},
  197–214. http://dx.doi.org/10.1037/0278-7393.32.2.197
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
    
}
\examples{
### Approximate replication of Figure 3 (top panel) in Oberauer (2006
data(oberauer06)
pd <- aggregate(rt ~ setsize+update_type+session, 
                data = oberauer06[which(oberauer06$update_type != "first item"),],
                FUN = mean)
bgk <- c("white","black")
plot(0,0, xlim=c(0,50),ylim=c(0,1800), type="n", las=1, xaxt="n", yaxt="n",
     xlab="Set size (Sessions 1-10)", ylab="RT (ms)",cex.lab=1.0,cex.axis=1.0)
title("Switch Costs across Sessions")
axis(1, at=c(1:50), labels = rep(1:5,10), lwd=0.5, lwd.ticks=0.2,cex.axis=0.5)
axis(2, at=seq(from=0,to=1800,by=200),lwd=1, lwd.ticks=1,cex.axis=1.0)
for (k in c(1:10)) {
  tbp <- pd[which(pd$session == k),]
  lines(c(((k-1)*5+1):((k-1)*5+5)),tbp$rt[1:5],lwd=1,lty=1)
  lines(c(((k-1)*5+2):((k-1)*5+5)),tbp$rt[6:9],lwd=1,lty=2)
  points(c(((k-1)*5+1):((k-1)*5+5)),tbp$rt[1:5],pch=22,bg=bgk[1],cex=0.5)
  points(c(((k-1)*5+2):((k-1)*5+5)),tbp$rt[6:9],pch=19,bg=bgk[2],cex=0.5)
}

legend(50,1800,c("no-switch", "switch"),lty=c(1:2),pch=c(22,19),pt.bg=bgk,cex=0.6,pt.cex=1.0, yjust = 1, xjust = 1)

}
\keyword{datasets}
