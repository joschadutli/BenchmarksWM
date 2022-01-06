\name{vergauwe15}
\alias{vergauwe15}
\docType{data}
\title{
  Vergauwe, Hartstra, Barrouillet, & Brass' (2015) Behavioral Data on Cognitive Load with
  Visuospatial and Verbal Stimuli
}
\description{
  These are the behavioral individual-trial data from Vergauwe et al. (2015). Stimuli from
  the memory task and the processing task were from the same domain (verbal or visual). The
  processing task either involved few or many items and was either easy or hard.
}
\usage{data("vergauwe15")}
\format{
  A data frame with 1728 observations on the following 12 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{domain}}{Task domain: \code{spatial} or \code{verbal}}
    \item{\code{condition}}{Item difficulty in processing task \code{easy} \code{hard}}
    \item{\code{num}}{Number of items in processing task: \code{verbal} --> (3 or 9); \code{spatial} --> (3 or 6)}
    \item{\code{probe}}{Which memory item was tested (Set size = 6)}
    \item{\code{proc.pc}}{Proportion correct responses in processing task}
    \item{\code{proc.meanRT}}{Mean response time to processing task}
    \item{\code{acc}}{Accuracy in recognition test (1/0)}
    \item{\code{rt}}{Response time to recognition test}
    \item{\code{totaltime}}{Total time available for processing task (constant at 9000 ms)}
    \item{\code{CL}}{Continuous Cognitive Load measure (\code{proc.meanRT}/(\code{totaltime}/\code{num}))}
  }
}
\details{
  For details, see Vergauwe et al. (2015).
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
  
  Vergauwe, E., Hartstra, E., Barrouillet, P., & Brass, M. (2015). Domain-general involvement
  of the posterior frontolateral cortex in Time-Based Resource-Sharing in working memory: An
  fMRI study. \emph{NeuroImage, 115}, 104-116. http://dx.doi.org/10.1016/j.neuroimage.2015.04.059
  
}
\examples{
### Two Figures that represent the data
data(vergauwe15)
pd <- aggregate(cbind(acc, CL) ~ domain + num,
                data = vergauwe15[which(vergauwe15$condition == "easy"),], FUN = mean)

plot(c(0.05,0.25), c(0.5,1), type = "n", xlab = "Cognitive Load (Mean RT / Total Time)",
     ylab = "Proportion correct", main = "Easy Processing Task", xaxt = "n")
axis(side = 1, at = c(0.05,0.1,0.15,0.2,0.25), labels = T)
points(x = pd$CL[pd$domain == "verbal"], 
       y = pd$acc[pd$domain == "verbal"], 
       type = "p", pch = 15, col = "red")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "verbal"),])$coefficients, 
       lty = 1, pch = 15, col = "red")

points(x = pd$CL[pd$domain == "spatial"], 
       y = pd$acc[pd$domain == "spatial"], 
       type = "p", pch = 17, col = "green")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "spatial"),])$coefficients, 
       lty = 3, pch = 17, col = "green")

legend(0.05, 0.5, c("verbal-many",
                   "spatial-many"), lty = c(1,3), 
       pch=c(15,17), col = c("red","green"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)

pd <- aggregate(cbind(acc, CL) ~  domain + num,
                data = vergauwe15[which(vergauwe15$condition == "hard"),], FUN = mean)

plot(c(0.05,0.25), c(0.5,1), type = "n", xlab = "Cognitive Load (Mean RT / Total Time)",
     ylab = "Proportion correct", main = "Hard Processing Task", xaxt = "n")
axis(side = 1, at = c(0.05,0.1,0.15,0.2,0.25), labels = T)
points(x = pd$CL[pd$domain == "verbal"], 
       y = pd$acc[pd$domain == "verbal"], 
       type = "p", pch = 15, col = "red")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "verbal"),])$coefficients, 
       lty = 1, pch = 15, col = "red")

points(x = pd$CL[pd$domain == "spatial"], 
       y = pd$acc[pd$domain == "spatial"], 
       type = "p", pch = 17, col = "green")
abline(lm(acc ~ CL, data = pd[which(pd$domain == "spatial"),])$coefficients, 
       lty = 3, pch = 17, col = "green")

legend(0.05, 0.5, c("verbal-many",
                    "spatial-many"), lty = c(1,3), 
       pch=c(15,17), col = c("red","green"),
       horiz = T, cex = 0.5, yjust = 0, xjust = 0)
}
\keyword{datasets}
