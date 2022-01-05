\name{adam15}
\alias{adam15}
\docType{data}
\title{
  Adam et al.'s (2015) set-size effect on accuracy in change detection
}
\description{
  Trial-level data for Adam et al.'s (2015) set-size effect on accuracy
  in change detection with arrays of coloured squares.
}
\usage{data("adam15")}
\format{
  A data frame with 12000 rows with the following 4 columns.
  \describe{
    \item{\code{subject}}{Unique ID number for each subject}
    \item{\code{size}}{Set size: 2-6}
    \item{\code{change}}{1 = a change occurred, 0 = no change}
    \item{\code{acc}}{1 = correct response, 0 = incorrect response}
  }
}
\details{

  These are the trial-level data collected by Adam et al. (2015,
  Experiment 1A, change-detection task), listed as benchmark dataset in
  working memory by Oberauer et al. (2018, BM1.1), and archived in
  Oberauer (2019). They comprise the complete dataset for 40 (presumably
  U.S.) adults.

  Participants were presented with arrays of 2-6 coloured squares for
  150ms, which disappeared for 900ms, followed by the presentation of
  one coloured square. Participants made an unspeeded button press to
  indicate whether the item had changed. There was a 900ms ITI.

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{ 

  Adam, K.C.S., Mance, I., Fukuda, K., & Vogel, E.K. (2015). The
  contribution of attentional lapses to individual differences in visual
  working memory capacity. \emph{Journal of Cognitive Neuroscience, 27},
  1601-1616.
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

}
\examples{

    ## Reproducing Figure 2F of Oberauer et al. (2018).
    data(adam15)
    library(tidyverse)

    adam15.sum <- adam15 \%>\%
        group_by(size) \%>\%
        summarise(acc = mean(acc))

    adam15.sum \%>\%
    ggplot(aes(x=size, y=acc)) +
        geom_point() + geom_line() + ylim(0.5,1)

}

\keyword{datasets}
