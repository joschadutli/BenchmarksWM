\name{oberauer01}
\alias{oberauer01}
\docType{data}
\title{
  Oberauer & Kliegl's (2001) set-size effect on accuracy and latency in
  WM updating  
}
\description{
  Item-level data for Oberauer & Kliegl's (2001) set-size effect
  on accuracy and latency in working memory updating, using simple arithemtic
  operations on digits.
}
\usage{data("oberauer01")}
\format{
  A data frame with 877084 rows with the following 9 columns.
  \describe{
    \item{\code{age}}{Age group of participant: 'young' adults, 'older' adults}
    \item{\code{subject}}{Unique ID number for each subject}
    \item{\code{part}}{Part of study: 1-2}
    \item{\code{size}}{Set size: 1-4 in study part 1, 4-6 in study part
      2}
    \item{\code{ptime}}{Presentation time of each update instruction
      (e.g "+3"), in milliseconds}
    \item{\code{trial}}{Trial number: 1-273. Trial number resets for
      each set size}
    \item{\code{item}}{Number of queried item within trial, ranges fom 1
      to the set size}
    \item{\code{measure}}{Type of data recorded in \code{val}: correct -
    Whether participant's response was correct (1) or incorrect (0);
    corrval - The correct response; resp - the participant's response;
    rt - the participant's response time, in milliseconds}
    \item{\code{val}}{Data point; type indicated by \code{measure}}
  }
}
\details{

  These are the item-level data collected by Oberauer & Kliegl (2001),
  listed as benchmark dataset in working memory by Oberauer et
  al. (2018, BM1.1, BM1.2), and archived in Oberauer (2019). They
  comprise the complete dataset for 18 young adults, and 18 older
  adults, from Germany. For both age groups, set sizes 1-4 were
  investigated in the first part of the experiment; in the second part,
  the set sizes were 4-6.

  Participants were presented with X digits in a circle. Each of
  these were then updated with a simple arithmetic operation
  (e.g. "+3"). The order of updates was always clockwise from the same
  starting point. Memory for each updated location was then probed, in a
  random order.

  The presentation times for each of the arithmetic operations varied
  according to an adaptive procedure. The maximum presentation time was
  6000 ms. Only these maximally long trials, from the young-adult group,
  form part of the benchmark reported in Oberauer et al. (2018, Figure
  2D).
  
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

  Oberauer, K., & Kliegl, R. (2001). Beyond resources: Formal models of
  complexity effects and age differences in working memory. \emph{The Euro-
  pean Journal of Cognitive Psychology, 13}, 187-215.
  
}
\examples{

    ## Reproducing Figure 2E of Oberauer et al. (2018).
    data(oberauer01)
    library(tidyverse)
    ## Filter and summarise data
    ober.acc <- oberauer01 \%>\%
        filter(age == "young") \%>\%
        filter(ptime > 5999) \%>\%
        filter(measure == "correct") \%>\%
        group_by(part, size) \%>\%
        summarise(acc = mean(val))
    ## Produce plot
    ober.acc \%>\%
        ggplot(aes(x=size, y=acc, color=factor(part))) +
        geom_point() + geom_line() + ylim(0.5,1)

}

\keyword{datasets}
