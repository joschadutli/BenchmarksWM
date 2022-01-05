\name{cowan02}
\alias{cowan02}
\docType{data}
\title{
  Cowan et al. (2002) Data on Output Order Effects on Accuracy
}
\description{
  Individual-trial data from Cowan et al. (2002). To deconfound input from output 
  order, Cowan et al. (2002) told participants to start serial recall at N and 
  serially recall the list to N-1. By deconfounding input from output order, 
  primacy effects are associated with output positions whereas recency effects 
  are determined by input positions.
}
\usage{data("cowan02")}
\format{
  A data frame with 34560 observations on the following 11 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{Modality}}{\code{V} = visual \code{A} = auditory}
    \item{\code{StimGroup}}{Where stimuli grouped: \code{G} or ungrouped: \code{U}}
    \item{\code{StartPos}}{N: Input position to start serial recall from}
    \item{\code{part}}{Partially recalled: \code{P} (3 digits) Whole array recalled: \code{W} (all 9 digits)}
    \item{\code{serpos}}{Serial Position}
    \item{\code{corr}}{Correct/Incorrect \code{1}/\code{0}}
    \item{\code{rt}}{Response time in ms}
    
  }
}
\details{
 For details, see Cowan et al. (2002).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Cowan, N., Saults, J. S., Elliott, E. M., & Moreno, M. V. (2002). 
  Deconfounding serial recall. \emph{Journal of Memory and Language, 46}, 153–177.
  http://dx.doi.org/10.1006/jmla.2001.2805
    
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

}
\examples{
data(cowan02)
## a Figure from ungrouped, visually presented stimuli:
u <- cowan02[which(cowan02$StimGroup == "U"),] 
uv <- u[which(u$Modality == "V"),]

pd <- aggregate(corr ~ StartPos*serpos, data = uv, FUN = mean)
bgk1 <- c("black","", "", "grey40", "","", "white")
bgk <- c("black","grey40", "white")

maxx <- 9
miny <- 0.0
plot(c(1,9),c(0,1), xlim=c(0, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Serial Position", ylab="Proportion Correct",cex.lab=1, cex.axis=1,
     main = "Ungrouped, visually pres. digits")
for (sp in c(1,4,7)) {
  pid <- subset(pd, StartPos==sp)
  lines(x = pid$serpos, 
        y = pid$corr, 
        type = "l", lty = 1)
  points(x = pid$serpos, 
         y = pid$corr, 
         pch = 21, bg = bgk1[sp])
}
legend(0,miny,c("N=1", "N=4", "N=7"), pch=21, pt.bg = bgk, xjust=0, yjust=0,
       title = "Start Pos", cex = 0.8)
}
\keyword{datasets}
