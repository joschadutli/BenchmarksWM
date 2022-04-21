\name{murdock70}
\alias{murdock70}
\docType{data}
\title{
  Murdock and Okada's (1970) Data on Inter-Response Latency in Free Recall
}
\description{
  Individual-item data from Murdock and Okada (1970) showing an increase in
  inter-response latency with output position in free recall. The recall data
  show a primacy but most notably a recency effect.
  The inter-response latencies (IRL) show an approximately exponential
  increase with output position when plotted dependent on correct
  recall (see plot 2 in Examples).
  Important note: The second plot reproduced in the examples section diverges
  from the originally published Fig. 1 in Murdock & Okada in some curves
  but not all. E.g., the IRL for 8 correctly recalled words at output
  position 8 should be larger than the IRL for 6 correctly recalled words
  at output position 6. Unfortunately, I was not able to reproduce this
  pattern from the data.
  One reason is that the original text doesn't make it explicit what
  the researchers did with cases when e.g., 4 words were recalled correctly,
  but that free recall was interspersed with incorrect recall. The plot
  in the example here excludes trials including incorrect recall, because
  doing so led to the highest similarity to the original plot.
}
\usage{data("murdock70")}
\format{
  A data frame with 18720 observations on the following 6 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant.}
    \item{\code{trial}}{Trial ID}
    \item{\code{output}}{Output Position: Ranging from 1 to 13.}
    \item{\code{input}}{Serial Position of recalled item: Rangeing from 1 to 20; 99 = extralist intrusion.}
    \item{\code{irl}}{Inter-response Latency measured in seconds}
    \item{\code{correct}}{Correct: Differentiates between correctly recalled items (
    \code{1} and extralist intrusions \code{0}}
    \item{\code{num_correct}}{Number of correctly recalled words in a trial (used for plotting).}
  }
}

\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Murdock, B. B., & Okada, R. (1970). Interresponse times in single-trial free
  recall. \emph{Journal of Experimental Psychology, 86}, 263-267.
  http://dx.doi.org/10.1037/h0029993

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
data(murdock70)
## a plot with the serial recall curve and one for inter-response latency by output position, dependent on correct recall:

## Serial Position Curve by Frequency
pd <- aggregate(correct ~ input, data = murdock70, FUN = function(x){sum(x)})
pd <- pd[which(pd$input != 0 & pd$input != 99), ]
plot(c(0,20),c(0,1300), xlim=c(1,20),ylim=c(0,1300), type="n", las=1,
     main = "Serial Position Curve by Frequency",
     xlab="Serial Position", ylab="Frequency of recall",cex.lab=1.2,cex.axis=1.)
lines(x = pd$input,
      y = pd$correct,
      type = "l", lty = 1)

## Inter-response Latency by Output Position dependent on correct recall
## Approximation of Fig. 1 in Murdock & Okada (1970)
library(tidyverse)

## prepare data
prep_pd <- murdock70 %>% filter(num_correct %in% 4:9) %>%
  group_by(subj, trial) %>%
  mutate(
    legal = case_when(max(output) > num_correct ~ 0,
                      TRUE ~ 1)
  )

next_prep <- prep_pd %>% filter(legal == 1) %>% filter(output > 1)

## aggreagte data
pdirl <- aggregate(irl ~ output + num_correct, data = next_prep, FUN = mean)
plot(c(1,9),c(0,12), xlim=c(1,9),ylim=c(0,13), type="n", las=1,
     main = "IRL partioned on total number of words recalled",
     xlab="Output Position", ylab="IRL (seconds)",cex.lab=1.2,cex.axis=1.)
## create lines with a for-loop
for (op in sort(unique(pdirl$num_correct))) {
  lines(x = pdirl$output[pdirl$num_correct == op],
        y = pdirl$irl[pdirl$num_correct == op],
        type = "l", lty = op-3)
  points(x = pdirl$output[pdirl$num_correct == op],
         y = pdirl$irl[pdirl$num_correct == op],
         pch = op+12, bg = "black")
}
legend(1,12,sort(unique(pdirl$num_correct)),lty=sort(unique(pdirl$num_correct))-3,pch=sort(unique(pdirl$num_correct))+12,pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=0)



}
\keyword{datasets}
