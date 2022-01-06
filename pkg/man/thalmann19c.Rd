\name{thalmann19c}
\alias{thalmann19c}
\docType{data}
\title{
  Data from Experiment 3 in Thalmann, Souza, & Oberauer's (2019) on the Effects of Chunking 
  in WM
}
\description{
  These are the individual-trial data from Experiment 3 in Thalmann, Souza, and Oberauer
  (2019). The procedure of Experiment 3 was exactly the same as in Experiment 2 (see 
  \code{\link{thalmann19b}}), except that participant were instructed to use articulatory
  suppression throughout list presentation.
}
\usage{data("thalmann19c")}
\format{
  A data frame with 24600 observations on the following 13 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Conditions (L1 = first list, i.e. row): \code{Singleton L1}, \code{Singleton L3}, \code{All New Lists}, \code{Chunk L1}, or \code{Chunk L3}}
    \item{\code{list.type}}{Type of list: \code{Singleton}, \code{New List}, or \code{Chunk}}
    \item{\code{list.in.pos}}{Input position of the list: \code{1}, \code{2}, or \code{3}}
    \item{\code{list.out.pos}}{Output position (at test) of the list: \code{1}, \code{2}, or \code{3}}
    \item{\code{inpos}}{Input position of single item}
    \item{\code{outpos}}{Output position of single item}
    \item{\code{list.acc}}{Accurate recall of whole list (i.e. row)}
    \item{\code{seracc}}{Accuracy in serial recall of single items}
    \item{\code{freeacc}}{Accuracy in free recall of single items}
    \item{\code{transposition}}{Transpositions of serial recall errors of single items}
    \item{\code{rt}}{Response time (s)}
  }
}
\details{
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
data(thalmann19c)
## Data for plot
pd <- aggregate(seracc ~ condition+list.type+inpos, data = thalmann19c, FUN = mean)
}
\keyword{datasets}
