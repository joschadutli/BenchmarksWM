\name{klauer04}
\alias{klauer04}
\docType{data}
\title{
  Klauer & Zhao's (2004) Data on the Double Dissociation between Visual and Spatial Working Memory
}
\description{
  Individual-trial data for Experiments 1, 2, and 4 in Klauer and Zhao (2004). The memory
  tasks and the processing tasks were manipulated within subjects. In the visual WM task,
  participants were asked to remember chinese ideographs. In the spatial WM task, stimuli
  were the location of dots on the screen. As processing tasks, participants either performed
  a color discrimination task (visual) or a movement discrimination task. Additionally, there
  was a condition with no processing task.
  
  The data show a double dissociation between visual and spatial memory tasks. Processing
  interferes more with memory when the two tasks are from the same domain.
}
\usage{data("klauer04")}
\format{
  A data frame with 6696 observations on the following 10 variables.
  \describe{
    \item{\code{exp}}{Experiments: \itemize{
    \item{\code{Exp1}: A 2 (vis. vs. spat. memory) x 3 (no vs. vis. vs. spat. processing) 
    x 3 (fast vs. medium vs. slow presentation times) design. Processing tasks were 
    performed during a 10s retention interval.}
    \item{\code{Exp2}: Same as \code{Exp1} except that the retention interval started with 
    a 500 ms blank screen before the processing task.}
    \item{\code{Exp4}: Same as \code{Exp2} except new ideographs in each trial and spatial 
    tapping as the spatial processing task.}}
    }
    \item{\code{subj}}{Unique subject ID within experiments.}
    \item{\code{trial}}{trial ID}
    \item{\code{memory_task}}{Memory Task: \code{locations} or \code{ideographs}}
    \item{\code{processing}}{Processing Task: \code{none}, \code{visual}, or \code{spatial}}
    \item{\code{domain}}{Two tasks from the \code{same} or \code{different} domains.}
    \item{\code{prestime}}{Presentation Time (ms)}
    \item{\code{num}}{Number of processing tasks solved in a trial (NA for no proc. task)}
    \item{\code{correct}}{Correctness of memory response (1/0)}
    \item{\code{acc}}{Accuracy of processing task (proportion correct in a trial)}
  }
}
\details{
  For a more detailed description of the experiments, see Klauer & Zhao (2004).
  The individual-trial data for the spatial tapping task in \code{Exp4} is missing. 
  Instead, the \code{acc} is the mean from all trials. The accuracy of this spatial
  tapping task was computed from long pauses which according to the authors were 
  rare. For all except one participant, this measure indicates no long pauses (i.e.
  \code{acc = 1.0}). Klauer & Zhao (2004) report an average tapping error of 2.6 percent
  which isn't present in the dataset.
}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Klauer, K. C., & Zhao, Z. (2004). Double dissociations in visual and spatial short-term
  memory. \emph{Journal of Experimental Psychology: General, 133}, 355–381. 
  http://dx.doi.org/10.1037/0096-3445.133.3.355
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
## Reproduce Figure 2 in Klauer & Zhao (2004)
data(klauer04)
d <- subset(klauer04, exp == "Exp1")
pd <- aggregate(correct ~ processing*memory_task, data = d, FUN = mean)
barspacing <- c(0.2,0.2,0.2,0.7,0.2,0.2)
legendtext <- c("None", "Spatial", "Visual", "None", "Spatial", "Visual")
bp = barplot(pd$correct, space = barspacing, col=c(rep("dark gray",3),rep("light gray",3)),
             ylab="Proportion correct", xlab="Distractor Task", axisnames=F, 
             ylim = c(0.6,1.0), xpd = F, main = "Effects of Task Domains")
text (2,.95,"Visual Memory",cex=1)
text (6,.95,"Spatial Memory",cex=1)
box()
axis(1, at = bp[,1], labels=legendtext, cex.axis=0.7)
}
\keyword{datasets}
