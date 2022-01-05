\name{shepherdson18}
\alias{shepherdson18}
\docType{data}
\title{Shepherdson, Oberauer, and Souza (2018) and Souza, Rerko, and Oberauer (2014) 
data on the effect of retro cues and the effect of set size on response times
%%   ~~ data name/kind ... ~~
}
\description{
  These are individual-trial data from Shepherdson et al. (2018) and Souza et al. 
  (2014) on the retro cue effect and the set size effect on response times and
  accuracy with letters (Exp 1), words (Exp2), and colored arrays (Exp S1a & S1b) 
  as stimulus materials. 
}
\usage{data("shepherdson18")}
\format{
  A data frame with 90240 observations on the following 8 variables.
  \describe{
    \item{\code{exp}}{
    \itemize{
    \item{\code{Exp 1} : Data of Experiment 1 in Shepherdson et al. (2018). Stimulus material = letters, data collected by Souza et al. (2014).}
    \item{\code{Exp 2} : Data of Experiment 2 in Shepherdson et al. (2018). Stimulus material = words.}
    \item{\code{Exp S1a} : Data of Experiment 1a in Souza et al. (2014). Stimulus material = colors.}
    \item{\code{Exp S1b} : Data of Experiment 1b in Souza et al. (2014). Stimulus material = colors.}
    }}
    \item{\code{subj}}{Unique ID number for each participant.}
    \item{\code{task}}{\itemize{\item{\code{vis} = visual (colors)} \item{\code{ver} = verbal (letters or words), see \code{exp}}}}
    \item{\code{size}}{Set size: 1,2,4,6. (2,4,6,8 in \code{Exp 2}).}
    \item{\code{probe}}{Recognition probe: \itemize{\item{\code{pos} = positive} \item{\code{int} = intrusion} \item{\code{neg} = negative (i.e. new probe)}} }
    \item{\code{CTI}}{\emph{Cue-test interval:} The time between retro cue presentation and probe presentation: \itemize{ \item{\code{No} = No cue} \item{\code{100} = 100 ms} \item{\code{400} = 400 ms} \item{\code{1000} = 1000 ms} \item{\code{2000} = 2000 ms.}}}
    \item{\code{correct}}{\itemize{\item{\code{1} = correct} \item{\code{0} = incorrect}}}
    \item{\code{rt}}{Response time (s)}
  }
}
\details{
  These are the individual-trial data of Experiments 1 & 2 in Shepherdson et al. 
  (2018) and Experiments 1A (S1a) & 1B (S1b) in Souza et al. (2014). \cr 
  Experiment 1 uses letters, Experiment 2 uses words and Experiments 1a & 1b use 
  colors as stimuli. In all Experiments the appearance of retro cues 
  was manipulated.
  In one condition there is no retro cue. In the other conditions, the time 
  available between presentation of the retro cue and the presentation of the 
  recognition probe was varied between 100 ms, 400 ms & 2000 ms. In Experiment 
  S1b there's an additional interval of 1000 ms.
  The set size was varied between 1, 2, 4 & 6 in Experiments 1, S1a & S1b and
  varied between 2, 4, 6 & 8 in Experiment 2.
  
  The "No Cue" condition shows a monotonic increase in response times with
  increasing set size. The retro
  cues lead to an impairment of this effect when there's enough time between 
  the cue and the recognition probe (400 ms, but especially 1000 ms & 2000 ms).
  In these cases, the response times are more or less constant for different
  set sizes.
  
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
  
  Shepherdson, P., Oberauer, K., & Souza, A. S. (2018). Working Memory Load
  and the Retro-Cue Effect: A Diffusion Model Account. \emph{Journal of Experimental
  Psychology: Human Perception and Performance, 44}, 286–310. 
  http://dx.doi.org/10.1037/xhp0000448
  
  Souza, A. S., Rerko, L., & Oberauer, K. (2014). Unloading and reloading working
  memory: Attending to one item frees capacity. \emph{Journal of Experimental 
  Psychology: Human Perception and Performance, 40}, 1237–1256.
  http://dx.doi.org/10.1037/a0036331
  
  
}
\examples{
  ### Reproduce part of Figure 2 from Shepherdson et al. (2018)
  ### without confidence interval
  
  data("shepherdson18")
  plotd <- shepherdson18[which(shepherdson18$exp == "Exp 1"), ]
  plotd <- aggregate(rt ~ size*CTI*subj, data=plotd, FUN=mean)
  plot <- interaction.plot(plotd$size, plotd$CTI, plotd$rt, type = "b",
                           pch= 15:19, xlab = "Set size", ylab = "Mean RT",
                           legend = F, xtick = T, ylim = c(0.4,1.3),
                           main = "Set size effect on RT with retro cues")
  legend(1, 0.8, c("No Cue", "100 ms", "2000 ms", "400 ms"), pch=15:19, yjust=0)
  ### watch out: legend is messy
}
\keyword{datasets}
