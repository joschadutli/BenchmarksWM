\name{cowan07}
\alias{cowan07}
\docType{data}
\title{
  Cowan and Morey's (2007) Data on the Effects of Multiple Memory Sets on Accuracy
}
\description{
  Individual-trial data from Cowan and Morey (2007). Depending on condition, participants
  were presented with one or two memory sets. These consisted either of three visual
  stiumli (colored squares) or five verbal stimuli (spoken letters or digits). In the 
  "single" condition, participants were only presented with one memory set from either 
  stimulus domain. In the "within" condition, participants were asked to remember two 
  memory sets of the same domain (e.g. a list of letters and a list of digits). In the 
  "cross" condition, one set of each domain were presented. Memory was tested with a 
  change detection task.
  
  As can be seen in the example (Figure 12 in Oberauer et al. (2012)), memorizing two
  sets from the same stimulus domain impaired WM more strongly than memorizing two sets
  from different domains compared to memorizing one memory set only.
}
\usage{data("cowan07")}
\format{
  A data frame with 4224 observations on the following 17 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant.}
    \item{\code{age}}{Age [YY;MM]}
    \item{\code{sex}}{Sex}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Condition: \itemize{
    \item{\code{"single"}: only one memory set presented}
    \item{\code{"cross"}: two memory sets from different domains presented}
    \item{\code{"within"}: two memory sets from the same domain presented}
    }}
    \item{\code{ISI}}{most probably a Cue-Test Interval (see Details)}
    \item{\code{listlength}}{List length of verbal stimuli (constant at 5)}
    \item{\code{setsize}}{Set size of visual stimuli (constant at 3)}
    \item{\code{serpos}}{Serial Position of tested verbal item (1-5)}
    \item{\code{order}}{Order of presentation of memory sets (e.g. "VisVerb")}
    \item{\code{cue}}{Cue: "First", "Second", or "Both" memory sets cued. (NA for "single" memory sets)}
    \item{\code{probe}}{Memory probe: "Vis" or "Verb"}
    \item{\code{testset}}{Tested memory set: "First" or "Second". Only relevant when "Both" sets were cued.}
    \item{\code{acc}}{Accuracy (1 = correct; 0 = incorrect)}
    \item{\code{corr_resp}}{Correct response: (y/n)}
    \item{\code{resp}}{Response (y/n)}
    \item{\code{rt}}{Response time (ms)}
  }
}
\details{
  From the dataset and the article (Cowan & Morey, 2007), it is not clear what the 
  variable "ISI" represents. Most likely, it is the cue-test interval as it is constantly
  3000 ms in the conditions involving two memory sets and varies between 3000 ms, 4000 ms,
  and 6000 ms in the "single" condition. This interpretation matches Figure 1 in Cowan and
  Morey (2007). 
  Some variables are only relevant in certain trials (i.e. certain combinations of conditions
  and stimulus domains) and are set to NA in those trial where they are irrelevant.
  For further details, see Cowan and Morey (2007).
}
\source{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Cowan, N., & Morey, C. C. (2007). How can dual-task working memory retention limits 
  be investigated? \emph{Psychological Science, 18}, 686-688. 
  http://dx.doi.org/10.1111/j.1467-9280.2007.01960.x
  
  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.
}
\examples{
### Reproduce Figure 12 in Oberauer et al. (2018)
data(cowan07)
pd = aggregate(acc ~ probe * condition, data = cowan07, FUN = mean)
pd$pcode[pd$probe == "Verb"] <- 1
pd$pcode[pd$probe == "Vis"] <- 2

plot(c(0.8,2.2), c(0.5,1.0), type = "n", xlab = "Tested Domain",
     ylab = "Proportion correct", main = "Multiple Memory Sets", xaxt = "n")
axis(side = 1, at = c(1,2), labels = c("Verb","Vis"))
lines(x = pd$pcode[pd$condition == "single"], y = pd$acc[pd$condition == "single"], 
      type = "b", lty = 1, pch = 15)
lines(x = pd$pcode[pd$condition == "cross"], y = pd$acc[pd$condition == "cross"], 
      type = "b", lty = 2, pch = 16)
lines(x = pd$pcode[pd$condition == "within"], y = pd$acc[pd$condition == "within"], 
      type = "b", lty = 3, pch = 17)
legend(1, 0.5, c("single", "cross", "within"), lty = 1:3, pch=15:17,
       horiz = FALSE, cex = 0.7, yjust = 0, xjust = 0)
}
\keyword{datasets}
