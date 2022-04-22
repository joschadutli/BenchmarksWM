\name{harvey07}
\alias{harvey07}
\docType{data}
\title{
  Harvey & Beaman's (2007) Data on the Modality Effect on Recency
}
\description{
  Individual-trial data collected and analyszed by Harvey and Beaman (2007) on
  the modality and recency effects in serial recall. Presentation and response
  modality vary.
}
\usage{data("harvey07")}
\format{
  A data frame with 1836 observations on the following 6 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant.}
    \item{\code{r_mode}}{Response modality: \code{written} \code{spoken}}
    \item{\code{modality}}{Presentation modality: \code{auditory} \code{visual}}
    \item{\code{material}}{Stimulus material: \code{numbers} \code{words}}
    \item{\code{serpos}}{Serial Position}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{
  These are the individual-subject data collected and analyzed by Harvey and Beaman
  (2007), listed as a benchmark dataset in Oberauer et al. (2018, BM3.2)
  and archived by Oberauer (2019). See Examples for a plot.
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Harvey, A. J., & Beaman, C. P. (2007). Input and output modality effects in
  immediate serial recall. \emph{Memory, 15}, 693-700.
  http://dx.doi.org/10.1080/09658210701644677

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(harvey07)

## Reproduce Figure 8 (a&b) in Oberauer et al. (2018)
d_writ <- harvey07[which(harvey07$r_mode == "written"),]
d_spok <- harvey07[which(harvey07$r_mode == "spoken"),]

par(mfrow=c(1,2))

pdw <- aggregate(acc ~ serpos*modality*material, data = d_writ, FUN = mean)

plot(c(1,9), c(0.0,1.05), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct",
     main = "Written response", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = TRUE)

lines(x = pdw$serpos[pdw$modality == "auditory" & pdw$material == "numbers"],
      y = pdw$acc[pdw$modality == "auditory" & pdw$material == "numbers"],
      type = "b", lty = 1, pch= 21, bg="black", col="black")
lines(x = pdw$serpos[pdw$modality == "auditory" & pdw$material == "words"],
      y = pdw$acc[pdw$modality == "auditory" & pdw$material == "words"],
      type = "b", lty = 2, pch= 22, bg="black", col="black")
lines(x = pdw$serpos[pdw$modality == "visual" & pdw$material == "numbers"],
      y = pdw$acc[pdw$modality == "visual" & pdw$material == "numbers"],
      type = "b", lty = 3, pch= 21, bg="grey", col="black")
lines(x = pdw$serpos[pdw$modality == "visual" & pdw$material == "words"],
      y = pdw$acc[pdw$modality == "visual" & pdw$material == "words"],
      type = "b", lty = 4, pch= 22, bg="grey", col="black")


### Figure 8b: Spoken response
pds <- aggregate(acc ~ serpos*modality*material, data = d_spok, FUN = mean)
plot(c(1,9), c(0.0,1.05), type = "n", xlab = "Serial Position",
     ylab = "Proportion correct",
     main = "Spoken response", xaxt = "n")
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9), labels = TRUE)

lines(x = pds$serpos[pds$modality == "auditory" & pds$material == "numbers"],
      y = pds$acc[pds$modality == "auditory" & pds$material == "numbers"],
      type = "b", lty = 1, pch= 21, bg="black", col="black")
lines(x = pds$serpos[pds$modality == "auditory" & pds$material == "words"],
      y = pds$acc[pds$modality == "auditory" & pds$material == "words"],
      type = "b", lty = 2, pch= 22, bg="black", col="black")
lines(x = pds$serpos[pds$modality == "visual" & pds$material == "numbers"],
      y = pds$acc[pds$modality == "visual" & pds$material == "numbers"],
      type = "b", lty = 3, pch= 21, bg="grey", col="black")
lines(x = pds$serpos[pds$modality == "visual" & pds$material == "words"],
      y = pds$acc[pds$modality == "visual" & pds$material == "words"],
      type = "b", lty = 4, pch= 22, bg="grey", col="black")
legend(2.5, 1.0, c("Auditory - numbers", "Auditory - words", "Visual - numbers",
                   "Visual - words"), lty = 1:4, pch = c(21,22,21,22),
       pt.bg = c("black","black","grey","grey"), horiz = FALSE, cex = 0.6,
       yjust = 1, xjust = 0)


}
\keyword{datasets}
