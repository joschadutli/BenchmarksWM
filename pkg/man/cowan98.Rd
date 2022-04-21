\name{cowan98}
\alias{cowan98}
\docType{data}
\title{
  Cowan et al. (1998) Data on the Relationship Between Verbal Processing Rates and 
  Short-Term Memory Span
}
\description{
  These are the individual-subject data from Cowan et al. (1998). The data show that both
  articulation speed and retrieval speed correlate with verbal Working Memory Capacity (WMC)
  measured with span tasks, but are not correlated with each other. This suggests that 
  verbal WMC depends on different processing rates rather than on a global processing rate.
}
\usage{data("cowan98")}
\format{
  A data frame with 172 observations on the following 18 variables.
  \describe{
    \item{\code{subj}}{Unique subect ID for each participant}
    \item{\code{age}}{Age (in years)}
    \item{\code{sex}}{Sex}
    \item{\code{ethnicity}}{Ethnicity (Caucasian, African American, Asian American, Unknown (UK))}
    \item{\code{digit.span}}{Digit span score}
    \item{\code{letter.span}}{Letter span score}
    \item{\code{letter.search.1}}{Memory search task with letters as stimuli and set size 1}
    \item{\code{digit.search.1}}{Memory search task with digits as stimuli and set size 1}
    \item{\code{letter.search.3}}{Memory search task with letters as stimuli and set size 3}
    \item{\code{digit.search.3}}{Memory search task with digits as stimuli and set size 3}
    \item{\code{letter.search.5}}{Memory search task with letters as stimuli and set size 5}
    \item{\code{digit.search.5}}{Memory search task with digits as stimuli and set size 5}
    \item{\code{letter.search.intercept}}{Intercept of the memory search tasks with letters}
    \item{\code{letter.search.slope}}{Slope of the memory search tasks with letters}
    \item{\code{digit.search.intercept}}{Intercept of the memory search tasks with digits}
    \item{\code{digit.search.slope}}{Slope of the memory search tasks with digits}
    \item{\code{rapid.alphabet}}{Rate in the rapid alphabet task (see Details)}
    \item{\code{rapid.counting}}{Rate in the rapid counting task (see Details)}
  }
}
\details{
  In the rapid alphabet task, participants silently repeated the alphabet (A-Z) in 60 s,
  making a pen mark each time the alphabet began. The measured rate is how many repititions 
  a participant was able to get through in the 60 seconds. The rapid counting task was exactly 
  the same except that participants had to repeatedly count from 1 to 10 in 30 seconds.
  For further details, see Cowan et al. (1998).
}
\source{
    
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{
  
  Cowan, N., Wood, N. L., Wood, P. K., Keller, T. A., Nugent, L. D., & Keller, 
  & C. V. (1998). Two separate  verbal  processing  rates  contributing  to  
  short-term memory  span.  \emph{Journal  of Experimental Psychology: General, 
  127}, 141-160. 10.1037//0096-3445.127.2.141 
  
}
\examples{
data(cowan98)
}
\keyword{datasets}
