\name{lange11}
\alias{lange11}
\docType{data}
\title{
  Lange, Cerella and Verhaeghen's (2011) Data on the Retrieval Benefit in
  Forward Serial Recognition
}
\description{
  Individual-item data from Lange, Cerella and Verhaeghen (2011, Exp 1, 2 and 4).
  The data show that recognition probes are most easily accessible in the order
  in which they have been presented. According to the data, the list position and
  not the spatial (i.e. visual) position is crucial. Additionally, recognizing
  an item within a list has a facilitating effect on correctly recognizing the
  next item in that list.
}
\usage{data("lange11")}
\format{
  A data frame with 95040 observations on the following 14 variables.
  \describe{
    \item{\code{id}}{Subject ID. Unique within one experiment.}
    \item{\code{task}}{Task: \code{1} = recognition memory task \code{0} = magnitude judgement task (see Lange et al. (2001))}
    \item{\code{con}}{Output-order condition:
    \itemize{
      \item \code{1}: forward
      \item \code{2}: fixed irregular (participants learned output order)
      \item \code{3}: backward
      \item \code{4}: random
    }}
    \item{\code{exp}}{Experiment: \code{1}, \code{2} or \code{4}}
    \item{\code{n_block}}{Block ID. Unique within one experiment.}
    \item{\code{trial}}{Trial ID. Unique within one experimental block.}
    \item{\code{size}}{Set Size}
    \item{\code{outnr}}{Output Position (always forward)}
    \item{\code{outpos}}{Input Position at Test (determined by condition (except random))}
    \item{\code{inpos}}{Spatial Input Position at Test (only varied in Exp. 2)}
    \item{\code{ptype}}{Recognition probe type:
    \itemize{
      \item \code{0}: positive probe
      \item \code{1}: intrusion negative probe
      \item \code{2}: extra-list negative probe
    }}
    \item{\code{magn}}{Response to magnitude judgment task:
    \itemize{
      \item \code{1}: larger 5
      \item \code{0}: smaller 5
    }}
    \item{\code{corr}}{Correct (1)}
    \item{\code{rt_raw}}{Raw response times (ms)}
    \item{\code{rt}}{Response times (ms) with outliers already set to \code{NA}.
    (Cut RT at individual mean + 3*sd and at RT < 100 ms)}
  }
}
\details{
  For more details on the magnitude judgment task and the different experiments, see
  Lange et al. (2011).
  The variable \code{outnr} is a form of serial position measure at test, i.e. it
  increseases step-wise from 1 to the set size and is probably only useful for
  plotting input positions by output positions.
  The variable \code{outpos} is the serial position an item had at input at the
  output position given by \code{outnr} at test. Thus, \code{outpos} is determined
  and varied by the experimental conditions.
  The variable \code{inpos} is only relevant in Experiment 2, where the authors
  manipulated the serial order of the spatial locations at input (and held them
  constantly in forward order at output) in order to disentangle spatial order from
  temporal serial order. \code{inpos} is equivalent to \code{outpos} except that it
  marks the input spatial location of the given item at test.
  For more details on the logic of spatial input and output positions, see Figure 1
  in Lange et al. (2011).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  Lange, E. B., Cerella, J., & Verhaeghen, P. (2011). Ease of access to list items
  in short-term memory depends on the order of the recognition probes. \emph{Journal
  of Experimental Psychology: Learning, Memory, and Cognition, 37}, 608-620.
  http://dx.doi.org/10.1037/a0022220

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{

data(lange11)
### Reproduction of Figure 2A (Exp. 1) in Lange et al. (2011)
plotd <- lange11[which(lange11$exp == 1),] # Experiment 1

taskd <- plotd[which(plotd$task == 1),] # Eecognition memory data only

pd_rt <- aggregate(rt ~ size*con, data = taskd, FUN = mean)

# Plotting
bgk <- c("black","grey40", "grey80", "white")
pt <- 21:24

maxx <- 5
miny <- 500
plot(c(3,5), c(500,1200), xlim=c(3, maxx), ylim=c(miny,1200), type="n", las=1,
     xlab="Set Size", ylab="Proportion Correct", cex.lab=1, cex.axis=1,
     main = "Response Times by Output-Order Condition and Set Size")

for (sp in 1:4) {
  pid <- subset(pd_rt, con==sp)
  lines(x = pid$size,
        y = pid$rt,
        type = "l", lty = 1)
  points(x = pid$size,
         y = pid$rt,
         pch = pt[sp], bg = bgk[sp])
}

legend(3, 1200, c("forward", "fixed-irr.", "backward", "random"), pch=pt,
       pt.bg = bgk, xjust=0, yjust=1, title = "Order Conditions", cex = 0.6)

}
\keyword{datasets}
