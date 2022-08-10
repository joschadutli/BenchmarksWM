\name{donkin12b}
\alias{donkin12b}
\docType{data}
\title{Donkin and Nosofsky's (2012b) effects of fast vs. slow presentation rate of the classic Sternberg (1966) task on response times (RT)

}
\description{
  Individual-subject data for Donkin & Nosofsky's (2012b) set size and serial
  position effects on response times in the classic Sternberg (1966) task.
  The effects differ for "fast" vs. "slow" presentation rates.
}
\usage{data("donkin12b")}
\format{
  A data frame with 140 observations on the following 5 variables.
  \describe{
    \item{\code{subject}}{Unique ID number for each subject}
    \item{\code{rate}}{Presentation rate: "fast" (subj 1-4) vs. "slow" (subj 5-7)}
    \item{\code{size}}{Set size: 1-5}
    \item{\code{pos}}{Serial position of recognition probe: 1-size, 6 for
    "new" probes}
    \item{\code{rt}}{Response time (s)}
  }
}
\details{

  These are the individual-subject data collected by Nosofsky et al. (2011;
  subjects 1-4) and Donkin & Nosofsky (2012b; subjects 5-7) analysed by
  Donkin & Nosofsky (2012b), listed as a benchmark dataset in Working Memory
  research by Oberauer et al. (2018). \cr
  A classic Sternberg (1966) task was
  completed with fast presentation rates and short retention intervals (RI) in
  one group (experiment 1; subjects 1-4; rate = 500 ms + 100 ms;
  RI = 400 ms) or with slow presentation rates and relatively long RIs in the
  other group of participants (experiment 2; subjects 5-7; rate = 1000 ms +
  200 ms; RI = 2000 ms). \cr
  Recognition response times for "fast" presentation rates are primarily
  determined by serial position (pos), whereas "slow" presentation rates
  lead to a recognition response time distribution that is mainly due
  to a effect of set sizes (Donkin & Nosofsky, 2012b).

}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
}
\references{
  Donkin, C., & Nosofsky, R. M. (2012b). The structure of short-term memory
  scanning: An investigation using response time distribution models. Psychonomic
  Bulletin & Review, 19, 363-394. http://dx.doi.org/ 10.3758/s13423-012-0236-8

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Sternberg, S. (1966). High-speed scanning in human memory. Science, 153, 652-654.
  http://dx.doi.org/10.1126/science.153.3736.652

}
\examples{
  data(donkin12b)

  ### Reproduce plots of Figure 3 in Oberauer et al. (2018)

  dplot <- aggregate(rt ~ pos*size*rate, data = donkin12b, FUN = mean)
  dplot$gv <- dplot$size
  dplot$gv[dplot$pos == 6] <- 6
  dfast <- dplot[which(dplot$rate == "fast"),]
  dslow <- dplot[which(dplot$rate == "slow"),]

  ### Example 1 (with ggplot)
  library(tidyverse)
  ggplot(data=dfast, mapping = aes(y=rt, x=as.factor(pos),
    group=interaction(size, gv), shape=as.factor(size))) +
    ggtitle("Fast presentation rate") +
    geom_point() + geom_line() + ylim(.35,.7) + ylab("RT (s)") +
    scale_x_discrete(name = "Serial Position",
                       labels=c("1","2","3","4","5","new"),
                       breaks=c(1,2,3,4,5,6)) +
    scale_shape_discrete(name="Set size")

  ggplot(data=dslow, mapping = aes(y=rt, x=as.factor(pos),
    group=interaction(size, gv), shape=as.factor(size))) +
    ggtitle("Slow presentation rate") +
    geom_point() + geom_line() + ylim(.35,.7) + ylab("RT (s)") +
    scale_x_discrete(name = "Serial Position",
                     labels=c("1","2","3","4","5","new"),
                     breaks=c(1,2,3,4,5,6)) +
    scale_shape_discrete(name="Set size")

  ### Example 2 (without ggplot)
  ### please ignore the line in Set size 5 to the "new" probe
  dfast$pos[dfast$pos == 6] <- "new"
  interaction.plot(dfast$pos, dfast$size, dfast$rt, type = "b", pch = 15:19,
                   ylim = c(.35,.70), xlab = "Serial Position",
                   ylab = "Mean RT", legend = FALSE, xtick=TRUE,
                   main = "Fast presentation rate")
  dslow$pos[dslow$pos == 6] <- "new"
  interaction.plot(dslow$pos, dslow$size, dslow$rt, type = "b", pch = 15:19,
                   ylim = c(.35,.70), xlab = "Serial Position",
                   ylab = "Mean RT", trace.label = "Set size",
                   main = "Slow presentation rate", xtick = TRUE)

}
\keyword{datasets}
