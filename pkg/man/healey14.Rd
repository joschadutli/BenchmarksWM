\name{healey14}
\alias{healey14}
\docType{data}
\title{
 Data from PEERS as documented and analyzed by Healey, Crutchley and Kahana (2014)
}
\description{
  For a description and other details, see Healey et al. (2014).
}
\usage{data("healey14")}
\format{
  A data frame with 27198 observations on the following 102 variables.
  \describe{
    \item{\code{subject}}{Unique subject ID for each participant.}
    \item{\code{session}}{Session ID}
    \item{\code{wordX}}{Identification number of word presented in list position X.}
    \item{\code{recwordY}}{Identification number of word recalled in output position Y.}
    
    \item{\code{recinposY}}{List position of word recalled in output position Y.}
    
    \item{\code{rectimeY}}{Cumulative response time of word recalled in output position Y.}
    
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM
  
}
\references{
  Healey, M. K., Crutchley, P., & Kahana, M. J. (2014). Individual differences 
  in memory search and their relation to intelligence. \emph{Journal of 
  Experimental Psychology: General, 143}(4), 1553. https://doi.org/10.1037/a0036306
}
\examples{
data(healey14)
}
\keyword{datasets}
