\name{barrouillet11}
\alias{barrouillet11}
\docType{data}
\title{
  Barrouillet, Portrat & Camos' (2011) Data on Cognitive Load Effects in Different Tasks
}
\description{
  Individual-subject data collected in Barrouillet, Portrat & Camos (2011). The data show
  Cognitive Load effects in all tasks.
}
\usage{data("barrouillet11")}
\format{
  A data frame with 262 observations on the following 7 variables.
  \describe{
    \item{\code{subj}}{Subject IDs. Unique within tasks.}
    \item{\code{task_domain}}{Task domain: \code{updating} \code{inhibition} \code{response selection} \code{retrieval}}
    \item{\code{task}}{Tasks: \code{0-back} \code{2-back} \code{ColorStroopColor} \code{ColorStroopNeutral} \code{NumberStroopNeutral} \code{NumberStroopNumber} \code{Parity-4} \code{Parity-6} \code{Parity-8} \code{Running Count} \code{Simple Storage} \code{Size-4} \code{Size-6} \code{Size-8}}
    \item{\code{totaltime}}{Total Time (ms)}
    \item{\code{prestime}}{Time on Task (ms)}
    \item{\code{CL}}{Continuous Cognitive Load Measure (\code{prestime}/\code{totaltime})}
    \item{\code{span}}{Mean Memory Span}
  }
}
\details{
  For further details, see Barrouilet et al. (2011).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  
  Barrouillet, P., Portrat, S., & Camos, V. (2011). On the law relating processing to 
  storage in working memory. \emph{Psychological Review, 118}, 175-192. 
  http://dx.doi.org/10.1037/a0022324
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{

### Reproduce Figure 2 in Barrouillet et al. (2011)
data(barrouillet11)
# markers corresponding to Barrouillet et al (2011)
pd <- aggregate(cbind(span, CL) ~ task, data = barrouillet11, FUN = mean)
pt = c(24, 24, 23, 23, 23, 23, 21, 21, 21, 24, 24, 22, 22, 22) 
col = rep("black", 14)
ptcol = c("red", "red", "blue", "blue", "blue", "blue", "black", "black",
          "black", "red", "red", "green", "green", "green")
xlim <- c(0,1)
ylim <- c(0,9)
plot(xlim, ylim, type = "n", xlab = "Cognitive Load", ylab = "Memory Span",
     main = "Cognitive Load in different Tasks", xaxt = "n")
axis(side = 1, at = 0:10/10, labels = TRUE)
points(x = pd$CL, y = pd$span, 
       type = "p", pch = pt, bg = ptcol)
abline(a=8.13, b=-8.33)
legend(1,9,c("Updating", "Inhibition", "Response Selection", "Retrieval"),
       pch=c(24,23,22,21), pt.bg=c("red", "blue", "green", "black"), yjust=1,
       xjust=1, cex = 0.7)
}
\keyword{datasets}
