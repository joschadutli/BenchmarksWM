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
  A data frame with over 1.2M observations on the following 6 variables.
  \describe{
    \item{\code{subject}}{Unique subject ID for each participant.}
    \item{\code{session}}{Session ID}
    \item{\code{listN}}{List ID}    
    \item{\code{type}}{Type of data: \code{word} = ID number of word presented to
      participant, \code{recword} = ID number of word recalled by participant,
      \code{rectime} = culumative response time of recall,
      \code{recinpos} = position of recalled word appeared in presented
      list; -1 if word was not presented.}
    \item{\code{pos}}{Position in list (presented or recalled, depending
      on \code{type}}
    \item{\code{value}}{Data point; either a word ID, a list position,
      or a time in milliseconds, depending on \code{type}}
  }
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
