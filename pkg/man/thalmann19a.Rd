\name{thalmann19a}
\alias{thalmann19a}
\docType{data}
\title{
  Data from Experiment 1 in Thalmann, Souza, & Oberauer's (2019) on the Effects of Chunking
  in WM
}
\description{
  These are the individual-trial data from Experiment 1 in Thalmann, Souza, and Oberauer
  (2019). Stimuli were words that were either new or pre-learned chunks (i.e. word pairs).
  In the experiment, words appeared in sequence in two rows on the screen (upper row first).
  In each row, either 2 or 4 words were presented in each trial. The data show that lists
  that contained chunks instead of new lists freed up WM capacity resulting in better
  memory performance in these trials. The size of the list containing chunks didn't matter.
}
\usage{data("thalmann19a")}
\format{
  A data frame with 30672 observations on the following 13 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{session}}{Session ID}
    \item{\code{trial}}{Trial ID}
    \item{\code{chunk_this}}{Type of tested list: \code{New List} or \code{Chunk}}
    \item{\code{chunk_other}}{Type of "other" list: \code{New List} or \code{Chunk}}
    \item{\code{setsize_this}}{Set size of tested list: \code{2} or \code{4}}
    \item{\code{setsize_other}}{Set size of "other" list: \code{2} or \code{4}}
    \item{\code{time_pres}}{Time of presentation of tested list: \code{Presentation First} or \code{Presentation Last}}
    \item{\code{time_rec}}{Time of test of tested list: \code{Recall First} or \code{Recall Last}}
    \item{\code{sp}}{Serial position: \code{1}, \code{2}, \code{3}, or \code{4}}
    \item{\code{seracc}}{Accuracy in serial recall (strict scoring)}
    \item{\code{rt}}{Response time (s)}
    \item{\code{freeacc}}{Accuracy in serial recall (lenient scoring)}
  }
}
\details{
  The dataframe contains two rows for each trial and serial position because in each trial,
  both of the presented lists were tested with serial recall. This structure was selected
  because one of the main results depends on the nature of the "other" list.
  For further details, see Thalmann, Souza, and Oberauer (2019).
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{

  Thalmann, M., Souza, A. S., & Oberauer, K. (2019). How does chunking help working memory?
  \emph{Journal of Experimental Psychology: Learning, Memory, and Cognition, 45}(1), 37-55.
  https://doi.org/10.1037/xlm0000578


}
\examples{
## Reproduction of Figure 1 in Thalmann, Souza, & Oberauer (2019)
data(thalmann19a)
pd <- aggregate(seracc ~ chunk_other+setsize_this+setsize_other,
                data = thalmann19a, FUN = mean)
pd$sso[pd$setsize_other == 2] <- 2
pd$sso[pd$setsize_other == 4] <- 4
p1 <- pd[which(pd$setsize_this == 2),]
p2 <- pd[which(pd$setsize_this == 4),]
par(mfrow=c(1,2))
plot(c(1,5), c(0.6,1.0), type = "n", xlab = "Size Other",
     ylab = "Proportion correct", main = "Size Tested = 2", xaxt = "n")
axis(side = 1, at = c(2,4), labels = c(2,4),
     cex.axis = 0.7)
lines(x = p1$sso[p1$chunk_other == "Chunk"],
      y = p1$seracc[p1$chunk_other == "Chunk"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = p1$sso[p1$chunk_other == "New List"],
      y = p1$seracc[p1$chunk_other == "New List"],
      type = "b", lty = 2, pch = 16, col = "red")
legend(1, 0.6, c("Chunk", "New List"), lty = 1:2,
       pch=15:17, col = c("blue","red"),
       horiz = FALSE, cex = 0.6, yjust = 0, xjust = 0, title = "Type Other:")
plot(c(1,5), c(0.6,1.0), type = "n", xlab = "Size Other",
     ylab = "", main = "Size Tested = 4", xaxt = "n")
axis(side = 1, at = c(2,4), labels = c(2,4),
     cex.axis = 0.7)
lines(x = p2$sso[p2$chunk_other == "Chunk"],
      y = p2$seracc[p2$chunk_other == "Chunk"],
      type = "b", lty = 1, pch = 15, col = "blue")
lines(x = p2$sso[p2$chunk_other == "New List"],
      y = p2$seracc[p2$chunk_other == "New List"],
      type = "b", lty = 2, pch = 16, col = "red")
}
\keyword{datasets}
