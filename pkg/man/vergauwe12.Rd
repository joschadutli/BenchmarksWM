\name{vergauwe12}
\alias{vergauwe12}
\docType{data}
\title{
   Vergauwe, Dewaele, Langerock, & Barrouillet's (2012) Data on the Resource-Sharing of 
   Working Memory Tasks involving Different Modalities
}
\description{
  Individual-subject data from Vergauwe et al. (2012). In Experiment 1, participants were 
  asked to remember auditorily presented words and to perform a visuospatial processing
  task between the presentation of to be remembered words. In Experiment 2, memory items 
  were visuospatial and the processing task involved auditorily presented verbal stimuli.
  Cognitive Load was manipulated by the number of processing tasks and the total time 
  available between the presentation of two consecutive memory items.
  The data show an effect of Cognitive Load in both experiments. According to the authors,
  these results suggest that visuospatial and verbal tasks rely on a general resource in
  Working Memory.
}
\usage{data("vergauwe12")}
\format{
  A data frame with 201 observations on the following 9 variables.
  \describe{
    \item{\code{exp}}{Experiment (1/2)}
    \item{\code{subj}}{Unique subject ID for each participant within an experiment}
    \item{\code{memory}}{Domain of memory items}
    \item{\code{processing}}{Domain of processing items}
    \item{\code{CL}}{Cognitive Load condition with levels: \code{low} \code{medium} \code{high}}
    \item{\code{totaltime}}{Total time available for a processing item}
    \item{\code{meanRT}}{Mean response time in the processing task}
    \item{\code{meanAcc}}{Mean accuracy of responses in the processing task}
    \item{\code{cogload}}{Continuous Cognitive Load measure (\code{meanRT}/\code{totaltime})}
    \item{\code{span}}{Memory span}
  }
}
\details{
  For the calculation of the memory \code{span} and other details, see Vergauwe et al. (2012)
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
  
  
  Vergauwe, E., Dewaele, N., Langerock, N., & Barrouillet, P. (2012). Evidence for a central
  pool of general resources in working memory. \emph{Journal of Cognitive Psychology, 24},
  359-366. http://dx.doi.org/10.1080/20445911.2011.640625
  
}  
\examples{
## Produce a Figure of the data of both experiments
data(vergauwe12)
pd <- aggregate(cbind(span, cogload) ~  CL + memory + processing, data = vergauwe12, FUN = mean)

plot(c(0.2,0.7), c(1.5,6.0), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.2,0.3,0.4,0.5,0.6,0.7), labels = TRUE)
points(x = pd$cogload[pd$memory == "verbal"], 
       y = pd$span[pd$memory == "verbal"], 
       type = "p", pch = 15, col = "red")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal"),])$coefficients, 
       lty = 1, pch = 15, col = "red")

points(x = pd$cogload[pd$memory == "spatial"], 
       y = pd$span[pd$memory == "spatial"], 
       type = "p", pch = 17, col = "green")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial"),])$coefficients, 
       lty = 3, pch = 17, col = "green")

legend(0.2, 1.5, c("verbal-spatial",
                   "spatial-verbal"), lty = c(1,3), 
       pch=c(15,17), col = c("red","green"),
       horiz = TRUE, cex = 0.5, yjust = 0, xjust = 0)

}
\keyword{datasets}
