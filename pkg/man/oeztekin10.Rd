\name{oeztekin10}
\alias{oeztekin10}
\docType{data}
\title{
  Oeztekin and McElree's (2010) Data on the Speed-Accuracy Trade-Off (SAT) Procedure and the Effects it shows in Working Memory Tasks
}
\description{
 Individual-trial data from Oeztekin and McElree (2010). The data show that the SAT
 approximates to an exponential approach to a limit. That is, more retrieval time 
 leads to rapid improvement of recognition memory for short retrieval intervals, 
 which finally reaches a limit of accuracy which cannot be improved by more 
 retrieval time. Secondly, the data show that participants recognition memory is 
 better and easier to retrieve (participants respond faster and more accurate) for 
 recently presented stimuli as opposed to stimuli presented early in the list 
 (Oberauer et al., 2018, BM3.3.2, Appendix B (Figure B1)).
}
\usage{data("oeztekin10")}
\format{
  A data frame with 54758 observations on the following 8 variables.
  \describe{
    \item{\code{id}}{Unique subject ID for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{lag}}{Index of lag in SAT: This variable can be used to calculate mean Lag + RT measures for each step of experimentally induced lag. Lags: 
    \itemize{
    \item{\code{0}}{43 ms}
    \item{\code{1}}{200 ms}
    \item{\code{2}}{300 ms}
    \item{\code{3}}{500 ms}
    \item{\code{4}}{800 ms}
    \item{\code{5}}{1500 ms}
    \item{\code{6}}{3000 ms}
    }}
    \item{\code{serpos}}{Serial Position}
    \item{\code{ptype}}{Probe type: \itemize{
    \item{\code{1}}{Positive probes}
    \item{\code{2}}{new lure}
    \item{\code{3}}{all lures from 1st category}
    \item{\code{4}}{all lures from 2nd category}
    \item{\code{5}}{recent negative lure from previous trial}
    }}
    \item{\code{lagrt}}{Lag + RT in \code{ms}: Experimentally induced lag + individual response time}
    \item{\code{rt}}{Individual-trial response time in \code{ms}}
    \item{\code{corr}}{Correct: \code{1} = correct \code{0} = incorrect}
  }
}
\details{
  For details on probe types, see Oeztekin & McElree (2010).
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
  
  Öztekin, I., & McElree, B. (2010). Relationship between measures of working 
  memory capacity and the time course of short-term memory retrieval and 
  interference resolution. \emph{Journal of Experimental Psychology: Learning, 
  Memory, and Cognition, 36}, 383–397. http://dx.doi.org/10.1037/a0018029
}
\examples{
data(oeztekin10)
## Vaguely reproduce Figure B1 in Oberauer et al. (2018)
SATpos <- subset(oeztekin10, ptype==1)

## Plot
bgk <- c("black", "grey20", "grey40", "grey60", "grey80", "white")

maxx <- 3.5
miny <- 0.4
plot(c(0,3.5),c(0,1), xlim=c(0, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Lag + RT (s)", ylab="Proportion Correct",cex.lab=1, cex.axis=1)
for (sp in 1:6) {
  sat <- subset(SATpos, serpos==sp)
  pd <- aggregate(corr ~ lag, data=sat, FUN=mean)
  pd_rt <- aggregate(lagrt ~ lag, data = sat, FUN = mean)
  pd_whole <- cbind(pd, pd_rt$lagrt)
  pd_whole$lagrt <- pd_whole$`pd_rt$lagrt`/1000
  lines(x = pd_whole$lagrt, 
        y = pd_whole$corr, 
        type = "l", lty = 2)
  points(x = pd_whole$lagrt, 
         y = pd_whole$corr, 
         pch = 21, bg = bgk[sp])
}
legend(maxx,miny,c("SP=1", "SP=2", "SP=3", "SP=4", "SP=5", "SP=6"), pch=21, pt.bg = bgk, xjust=1, yjust=0)
}
\keyword{datasets}
