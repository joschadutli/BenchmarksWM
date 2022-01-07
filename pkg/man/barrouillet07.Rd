\name{barrouillet07}
\alias{barrouillet07}
\docType{data}
\title{
 Barrouillet et al. (2007) Data on the Cognitive Load Effect in Complex Span Tasks
}
\description{
  Individual-subject data from Experiment 3 in Barrouillet et al. (2007). Three levels of
  Cognitive Load (low, medium, high) and the type of processing task (parity, location) 
  were manipulated between subjects. Memoranda were letters and the processing tasks had
  digits as stimulus material. 
  The data show a decline in the memory span measure with increasing Cognitive Load. 
}
\usage{data("barrouillet07")}
\format{
  A data frame with 96 observations on the following 8 variables.
  \describe{
    \item{\code{subj}}{Subject IDs that range from 1 to 16 in each of the 6 experimental groups. Accordingly, there are 6 subjects with ID = 1, etc.}
    \item{\code{task}}{Processing task: \code{parity} (odd/even-judgement) or \code{location} (above/below-screen-center-judgement)}
    \item{\code{cogload}}{Cognitive Load conditions with levels \code{high} < \code{medium} < \code{low}}
    \item{\code{CL}}{Computed Cognitive Load: "Time on Task"/"Total Time"}
    \item{\code{tasktime}}{Time on Task (Total is constant: 6900 ms)}
    \item{\code{series_completed}}{Amount of completed series. See Barrouillet et al. (2007)}
    \item{\code{span}}{Mean Memory Span (amount of items remembered on average)}
    \item{\code{pctNR}}{I don't know yet what that is. pct stands for percentage.}
  }
}
\details{
  For further details, see Barrouillet et al. (2007).
}
\source{
   
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Barrouillet, P., Bernardin, S., Portrat, S., Vergauwe, E., & Camos, V. (2007). Time 
  and cognitive load in working memory. \emph{Journal of Experimental Psychology: Learning,
  Memory, and Cognition, 33}, 570-585. http://dx.doi.org/10.1037/0278-7393.33.3.570
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Approximate reproduction of Figure 1 in Barrouillet et al. (2007)
data(barrouillet07)
pd <- aggregate(cbind(span, CL) ~ task + cogload, data = barrouillet07, FUN = mean)
plot(c(0.25,0.6), c(3.0,6.5), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6), labels = TRUE)
lines(x = pd$CL[pd$task == "parity"], y = pd$span[pd$task == "parity"], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$CL[pd$task == "location"], y = pd$span[pd$task == "location"], 
      type = "b", lty = 2, pch = 17)
abline(lm(span ~ CL, data = pd[which(pd$task == "parity"),])$coefficients, 
       lty = 1, pch = 15)
abline(lm(span ~ CL, data = pd[which(pd$task == "location"),])$coefficients,
       lty = 2, pch = 16)
legend(0.6, 6.5, c("Parity", "Location"), lty = 1:4, pch=15:18, title = "Task:",
       horiz = FALSE, cex = 0.7, yjust = 1, xjust = 1)
}
\keyword{datasets}
