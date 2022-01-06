\name{thalmann19d}
\alias{thalmann19d}
\docType{data}
\title{
  Data from Experiment 4 in Thalmann, Souza, & Oberauer's (2019) on the Effects of Chunking 
  in WM
}
\description{
  These are the individual-trial data from Experiment 3 in Thalmann, Souza, and Oberauer
  (2019). Participants were presented with letters in a 3x3 grid where each row was considered
  as a list. Lists could consist of known chunks (e.g. FBI) or new letters (i.e. letters not
  constituting a known chunk). Chunks were either presented in the first, the second, the
  third row or not at all. The data show that chunks improve recall of later presented list,
  but also some facilitation (or freed up capacity) for earlier presented lists.
}
\usage{data("thalmann19d")}
\format{
  A data frame with 34560 observations on the following 12 variables.
  \describe{
    \item{\code{subj}}{Unique subject ID for each participant}
    \item{\code{trial}}{Trial ID}
    \item{\code{condition}}{Conditions (L1 = first list, i.e. row): \code{Chunk L1}, \code{Chunk L2}, \code{Chunk L3}, or \code{All New Lists}}
    \item{\code{list.type}}{type of list: \code{Chunk} or \code{New List}}
    \item{\code{list.in.pos}}{Input position of the list: \code{1}, \code{2}, or \code{3}}
    \item{\code{list.out.pos}}{Output position (at test) of the list: \code{1}, \code{2}, or \code{3}}
    \item{\code{inpos}}{Input position of single item}
    \item{\code{outpos}}{Output position of single item}
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
pd <- aggregate(seracc ~ condition+list.type+inpos, data = thalmann19d, FUN = mean)
}
\keyword{datasets}
