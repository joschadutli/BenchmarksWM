\name{thalmann19b}
\alias{thalmann19b}
\docType{data}
\title{
  Data from Experiment 2 in Thalmann, Souza, & Oberauer's (2019) on the Effects of Chunking 
  in WM
}
\description{
  These are the individual-trial data from Experiment 2 in Thalmann, Souza, and Oberauer
  (2019). Participants were presented with letters in a 3x3 grid where each row was considered
  as a list. Lists could consist of known chunks (e.g. FBI), singletons (i.e. only 1 letter 
  in the last of the three boxes), or new letters (i.e. letters not constituting a known 
  chunk). Chunks or singletons were either presented in the first row or the last row (and 
  therefore, at the beginning or the end of the list). Each list (i.e. row) had to be 
  serially recalled but the order of the lists at test was varied over trials.
  The data show that chunks are better recalled than new lists and that chunks improved
  recall of later presented new lists but not recall of earlier presented new lists compared
  to when all lists were new.
}
\usage{data("thalmann19b")}
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
data(thalmann19b)
## maybe str(thalmann19b) ; plot(thalmann19b) ...
}
\keyword{datasets}
