\name{chen09}
\alias{chen09}
\docType{data}
\title{
  Chen & Cowan's (2009) dataset on the core verbal working memory capacity. 
}
\description{
  Individual-trial data from Chen & Cowan (2009) on verbal working memory 
  capacity. Participants memorized either new single words, studied single words
  or studied word-pairs. When articulatory rehearsal is suppressed, participants
  recall about 3 chunks (i.e. single words or studied word-pairs). \cr
  The data are subdivided into two groups. Participants of one group remained
  silent during the tasks. Participants of the other group had to engage in 
  articulatory suppression to block chunking.
}
\usage{data("chen09")}
\format{
  A data frame with 650 observations on the following 5 variables.
  \describe{
    \item{\code{group}}{Group: \itemize{\item{\code{2}= no AS} \item{\code{4}= AS (AS = articulatory suppression)}}}
    \item{\code{subj}}{Unique ID number for each subject}
    \item{\code{AS}}{Articulatory suppression (identical to \code{group}): "silent" vs. "AS"}
    \item{\code{condition}}{Conditions: \itemize{\item{Digits indicate number of chunks presented} \item{\code{n}: new single words} \item{\code{s}: studied single words} \item{\code{p}: studied word-pairs (2-word chunks)}}}
    \item{\code{span}}{correctly recalled chunks (regardless of serial order)}
  }
}
\details{
  These are the individual-trial data collected and analyzed by Chen & Cowan 
  (2009), listed as a benchmark finding in working memory research by Oberauer 
  et al. (2018). The findings suggest that there is a core verbal working memory 
  capacity of about three chunks in young adults. 
  
  When articulatory rehearsal was suppressed (group == 4) and no new chunks could
  be formed by participants, the number of correctly recalled chunks was found to
  be constant over conditions and list lengths. 
  
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Chen, Z., & Cowan, N. (2009). Core verbal working-memory capacity: The limit in
  words retained without covert articulation. \emph{Quarterly Journal of 
  Experimental Psychology: Human Experimental Psychology, 62}, 1420-1429. 
  http://dx.doi.org/10.1080/17470210802453977
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
  
}
\examples{
  ### reproduce Figure 4 in Oberauer et al. (2018)
  data("chen09")
  dAS <- subset(chen09, group==4) # data where rehearsal is articulatory suppressed
  plot_data <- aggregate(span ~ condition, data = dAS, FUN = mean)
  plot_data <- plot_data[c(1,9,6,3,2,10,8,5,7,4),]
  t <- as.table(plot_data$span)
  names(t) <- plot_data$condition
  barplot(t, main = "Figure 4: Numbers of Chunks recalled",
        xlab = "# = number of chunks presented; n = new single words; 
        s = pre-trained single words; p = pre-trained word pairs (chunks)", 
        ylab = "Number of chunks recalled", ylim = c(0,4))
}
\keyword{datasets}
