\name{vergauwe10}
\alias{vergauwe10}
\docType{data}
\title{
  Vergauwe, Barrouillet, and Camos' (2010) Data on Cognitive Load Effects in Dual-Task Situations.
}
\description{
  Individual-subject data from Vergauwe et al. (2010). Participants maintained verbal or
  visuospatial materials while also performing a verbal or visuospatial processing task.
  Depending on Cognitive Load condition, participants had more or less free time during
  the processing tasks.
  The data show that Cognitive Load has an effect on memory performance when the memory
  task and the processing task involve stimuli from different domains.
}
\usage{data("vergauwe10")}
\format{
  A data frame with 288 observations on the following 9 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{memory}}{Type of memory items}
    \item{\code{processing}}{Type of processing items}
    \item{\code{CL}}{Cognitive load condition with levels: \code{low} \code{medium} \code{high}}
    \item{\code{totaltime}}{Total time available per processing item (ms)}
    \item{\code{meanRT}}{Mean response times to processing items (ms)}
    \item{\code{meanAcc}}{Mean accuracy to processing items}
    \item{\code{cogload}}{Continuous Cognitive Load measure (\code{meanRT}/\code{totaltime}) see Details}
    \item{\code{span}}{Memory span}
  }
}
\details{
  The continuous cognitive load measure (\code{cogload}) deviates from the
  reported values in the article. In the original data, the cognitive load
  measure was calculated as the ratio of the sum of the processing time in a
  trial to the total time available. It appears, this was done on an
  individual-trial level. In the data avialable here, the data is on
  the level of conditions and thus, the processing times are already
  aggregated. Therefore, the cognitive load measure is slightly different
  from the reported one in Vergauwe et al. (2010). The cognitive load
  effect and its nature are intact, however.
  For the definition and calculation of the memory \code{span} measure, see Vergauwe,
  Barrouillet, and Camos (2010).
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

  Vergauwe, E., Barrouillet, P., & Camos, V. (2010). Do mental processes share a
  domain-general resource? \emph{Psychological Science, 21}, 384-390.
  http://dx.doi.org/10.1177/0956797610361340
}
\examples{
### Approximately reproduce Fig. 1 in Vergauwe et al. (2010)
### (CogLoad measure is different from publication.
### Cognitive load effect is the same. See Details
### section for a description of the reason.)
data(vergauwe10)

pd <- aggregate(cbind(span, cogload) ~  CL + memory + processing, data = vergauwe10, FUN = mean)

plot(c(0.2,0.7), c(1.5,6.0), type = "n", xlab = "Total Procesing Time / Total Time",
     ylab = "Mean Span", main = "Effects of Cognitive Load", xaxt = "n")
axis(side = 1, at = c(0.2,0.3,0.4,0.5,0.6,0.7), labels = TRUE)
points(x = pd$cogload[pd$memory == "verbal" & pd$processing == "verbal"],
      y = pd$span[pd$memory == "verbal" & pd$processing == "verbal"],
      type = "p", pch = 15, col = "red")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal" &
                                            pd$processing == "verbal"),])$coefficients,
       lty = 1, pch = 15, col = "red")
points(x = pd$cogload[pd$memory == "verbal" & pd$processing == "spatial"],
      y = pd$span[pd$memory == "verbal" & pd$processing == "spatial"],
      type = "p", pch = 16, col = "blue")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "verbal" &
                                            pd$processing == "spatial"),])$coefficients,
       lty = 2, pch = 16, col = "blue")
points(x = pd$cogload[pd$memory == "spatial" & pd$processing == "verbal"],
      y = pd$span[pd$memory == "spatial" & pd$processing == "verbal"],
      type = "p", pch = 17, col = "green")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial" &
                                            pd$processing == "verbal"),])$coefficients,
       lty = 3, pch = 17, col = "green")
points(x = pd$cogload[pd$memory == "spatial" & pd$processing == "spatial"],
      y = pd$span[pd$memory == "spatial" & pd$processing == "spatial"],
      type = "p", pch = 18, col = "black")
abline(lm(span ~ cogload, data = pd[which(pd$memory == "spatial" &
                                            pd$processing == "spatial"),])$coefficients,
       lty = 4, pch = 18, col = "black")
legend(0.2, 1.5, c("verbal-verbal", "verbal-spatial",
                   "spatial-verbal", "spatial-spatial"), lty = 1:4,
       pch=15:18, col = c("red","blue","green","black"),
       horiz = TRUE, cex = 0.5, yjust = 0, xjust = 0)

}
\keyword{datasets}
