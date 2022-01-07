\name{hedge13}
\alias{hedge13}
\docType{data}
\title{
  Data from Hedge & Leonards (2013) on Item-Switch Effects
}
\description{
  These are the individual-trial behavioral data from Experiments 1 and 2 in Hedge and
  Leonards (2013). Each trial started with the presentation of a blue and a red circle
  in different locations in a 3x3 grid. During the trial sequence, red or blue arrows appeared
  in the center of the grid and participants had to mentally update the position of the 
  circles by moving the location one grid position into the direction of the presented arrow.
  Updating steps were defined as "repeat" when two consecutive steps were on the same object
  (e.g. two blue arrows in row), or as "switch" when two consecutive steps were on a different
  object.  
  The data show a switch-cost (i.e. a repitition benefit) in the speed of the updating
  process. Experiments 1 and 2 are identical except that in Experiment 2, participants
  had to return their fixation to the center square after each updating step.
  
}
\usage{data("hedge13")}
\format{
  A data frame with 22176 observations on the following 9 variables.
  \describe{
    \item{\code{exp}}{Experiment (1 or 2)}
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{block}}{Block ID (Block 1 was a practice run and excluded from analysis)}
    \item{\code{trial}}{Trial ID (1-9 in each block)}
    \item{\code{length}}{Length of the trial sequence (9, 11, or 13)}
    \item{\code{serpos}}{Serial Position (or updating step)}
    \item{\code{update_type}}{Type of updating step: \itemize{ \item{\code{first update}}{(excluded from analysis)} \item{\code{repeat}} \item{\code{switch}}}}
    \item{\code{acc}}{Accuracy: Correct (1) when final locations of both stimuli were reported correctly. (0 = incorrect; incorrect trials were excluded from analysis.)}
    \item{\code{rt}}{Response time (s)}
  }
}
\details{
  For further details and report of the eye-tracking data, see Hedge and Leonards (2013).
}
\source{
    Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Hedge, C., & Leonards, U. (2013). Using eye movements to explore switch costs in working
  memory. \emph{Journal of Vision, 13}(4):18, 1-19. http://dx.doi.org/10.1167/13.4.18
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
## Simple barplot
data(hedge13)
### Simple barplot
hedge13included <- hedge13[which(hedge13$block != 1 & hedge13$update_type != "first update"
                                 & hedge13$acc == 1),]
pd <- aggregate(rt ~ update_type+exp, data = hedge13included, FUN = median, na.rm=TRUE)
bp = barplot(pd$rt, space = c(0.2,0.2,0.7,0.2), col=c("lightgrey", "darkgrey", 
                                                      "lightgrey", "darkgrey"),
             ylab="Median RT", xlab="Update Type", axisnames=FALSE, ylim = c(0,1.5))
text (1.3,1.3,"Experiment 1",cex=0.8)
text (4.2,1.3,"Experiment 2",cex=0.8)
box()
axis(1, at = bp[,1], labels=c("repeat", "switch", "repeat", "switch"), cex.axis=0.7)
}
\keyword{datasets}
