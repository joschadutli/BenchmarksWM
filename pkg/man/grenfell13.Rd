\name{grenfell13}
\alias{grenfell13}
\docType{data}
\title{
  Grenfell-Essam, Tan, & Ward's (2013) Data on th Effects of Presentation Duration
  and Rehearsal
}
\description{
  These are the individual-response data from Experiment 1 in Grenfell-Essam et
  al. (2013). The data show that for long lists of words, having time for
  articulatory rehearsal has an effect on the probability that the first item 
  is recalled correctly. Articulatorily suppressing rehearsal eliminates this effect.
}
\usage{data("grenfell13")}
\format{
  A data frame with 47520 observations on the following 9 variables.
  \describe{
    \item{\code{Include}}{Were the data included in the analysis? \code{1} = yes
    \code{2} = no}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trialnumber}}{Trial number}
    \item{\code{Suppression}}{\code{"AS"} = Articulatory Suppression, 
    \code{"No AS"} = Silent}
    \item{\code{prestime}}{Presentation duration: \code{0.5 s}, \code{1 s},
    \code{3 s}}
    \item{\code{length}}{Length of presented list of words: \code{2,4,5,6,7,8,10,12}}
    \item{\code{serpos}}{Serial position}
    \item{\code{FRcorrect}}{Correct recall of first item (\code{1 vs. 2})}
    \item{\code{corr}}{Correct score; regardless of recall order}
  }
}
\details{
   These are the individual-response data from Experiment 1 in Grenfell-Essam et
   al. (2013), archived by Oberauer (2019). Articulatory Suppression was manipulated
   between-subjects. List length and presentation duration of each presented word
   was manipulated within-subject.
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Grenfell-Essam, R., Ward, G., Tan, L. (2013). The Role of Rehearsal on the 
  Output Order of Immediate Free Recall of Short and Long Lists. \emph{Journal of
  Experimental Psychology: Learning, Memory, and Cognition, 39}, 317-347. 
  http://dx.doi.org/10.1037/a0028974
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(grenfell13)
## create a plot!
## plots
dincl <- grenfell13[which(grenfell13$Include == 1),]
dAS <- dincl[which(dincl$Suppression == "AS"),] 
dSilent <- dincl[which(dincl$Suppression == "No AS"),]

par(mfrow=c(1,2))
#silent
splot <- aggregate(FRcorrect ~ length*prestime, data = dSilent, FUN = mean)
plot(c(2,12), c(0,1.1), type = "n", xlab = "List length",
     ylab = "P(first item recalled correctly)", main = "Silent", xaxt = "n")
axis(side = 1, at = c(2,4,5,6,7,8,10,12), labels = T)
lines(x = splot$length[splot$prestime == 0.5], 
      y = splot$FRcorrect[splot$prestime == 0.5], 
      type = "b", lty = 1, pch = 15)
lines(x = splot$length[splot$prestime == 1], 
      y = splot$FRcorrect[splot$prestime == 1], 
      type = "b", lty = 2, pch = 16)
lines(x = splot$length[splot$prestime == 3], 
      y = splot$FRcorrect[splot$prestime == 3], 
      type = "b", lty = 3, pch = 17)
legend(2, 0, c("0.5 s / item", "1 s / item", "3 s / item"), lty = 1:3, pch=15:17,
       title = "Pres. Time:", horiz = F, cex = 0.5, yjust = 0, xjust = 0)

ASplot <- aggregate(FRcorrect ~ length*prestime, data = dAS, FUN = mean)
plot(c(2,12), c(0,1.1), type = "n", xlab = "List length",
     ylab = "P(first item recalled correctly)", main = "Articulatory Suppression", xaxt = "n")
axis(side = 1, at = c(2,4,5,6,7,8,10,12), labels = T)
lines(x = ASplot$length[ASplot$prestime == 0.5], 
      y = ASplot$FRcorrect[ASplot$prestime == 0.5], 
      type = "b", lty = 1, pch = 15)
lines(x = ASplot$length[ASplot$prestime == 1], 
      y = ASplot$FRcorrect[ASplot$prestime == 1], 
      type = "b", lty = 2, pch = 16)
lines(x = ASplot$length[ASplot$prestime == 3], 
      y = ASplot$FRcorrect[ASplot$prestime == 3], 
      type = "b", lty = 3, pch = 17)

}
\keyword{datasets}
