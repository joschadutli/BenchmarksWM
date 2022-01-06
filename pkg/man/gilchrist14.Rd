\name{gilchrist14}
\alias{gilchrist14}
\docType{data}
\title{Gilchrist and Cowan's (2014) set-size effect on response times 
}
\description{
  Individual-trial data for Gilchrist & Cowan's (2014) set-size effect
  on response times in a change detection task. Probes were either displayed at
  the center of the screen or at the actual stimulus location.
}
\usage{data("gilchrist14")}
\format{
  A data frame with 33885 observations on the following 8 variables.
  \describe{
    \item{\code{exp}}{Experiment 1 or 2}
    \item{\code{subject}}{Unique ID number for each subject}
    \item{\code{order}}{Ascending or descending set size within a block of trials}
    \item{\code{location}}{Change detection probe presented central or at 
    location}
    \item{\code{size}}{Set-size: 1-6}
    \item{\code{change}}{Changed: 0=old, 1=new}
    \item{\code{acc}}{Accuracy: 0=incorrect, 1=correct}
    \item{\code{rt}}{Response time (ms)}
  }
}
\details{
  These are the individual-trial data collected by Gilchrist & Cowan (2014),
  whose finding of a monotonic increase in response times as a function of
  set size in change detection in visual Working Memory was listed as a benchmark
  in Oberauer et al. (2018, B1.2). The dataset consists of data from two 
  experiments. Data of Experiment 1 includes accuracy and response time
  measures for 31 U.S. university students. Data of Experiment 2 contains
  accuracy and response times for 28 U.S. university students.
  
  The original experiments were designed to examine the effect of presenting
  change detection probes in the same location as in item presentation as
  opposed to presenting these probes in the screen center (variable: location).
  
  Note:\cr
  In contrast to the reported data in Gilchrist & Cowan (2014), the to-be-
  excluded participant in the data of Experiment 1 could not be identified
  because the number of blocks has not been recorded and therefore, there is 
  an additional participant in the dataset. Additionally, in the data for 
  Experiment 2, applying the reported criterion led to exclusion of data 
  of 4 participants instead of the reported 3.
  
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Gilchrist, A. L., & Cowan, N. (2014). A two-stage search of visual working 
  memory: Investigating speed in the change-detection paradigm. \emph{Attention,
  Perception & Psychophysics, 76}, 2031-2050.
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
  ### Reproduce plot of Figure 4 in Gilchrist & Cowan (2014).
  ### Set size effect on response times for correct responses.
  ### (without confidence intervals)

  data(gilchrist14)
  library(tidyverse)
  exp2 <- gilchrist14[which(gilchrist14$exp == 2),]
  exp2$plotv[exp2$location == "Central" & exp2$change == 0] <- "Central-Old"
  exp2$plotv[exp2$location == "Central" & exp2$change == 1] <- "Central-New"
  exp2$plotv[exp2$location == "Location" & exp2$change == 0] <- "Location-Old"
  exp2$plotv[exp2$location == "Location" & exp2$change == 1] <- "Location-New"
  
  exp2 <- exp2[which(exp2$acc == 1),]
  
  plotd <- aggregate(rt ~size*plotv, data = exp2, FUN = mean, na.rm=TRUE)
  plotd \%>\% ggplot(aes(x=size, y=rt, color=plotv)) +
    geom_point() + geom_line() + 
    scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
    ylim(600,1100) +
    xlab("Set size") +
    ylab("Mean RT (ms)") +
    theme(legend.title = element_blank())
    

}
\keyword{datasets}
