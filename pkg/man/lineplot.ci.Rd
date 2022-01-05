\name{lineplot.ci}
\alias{lineplot.ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plotting lines with confidence interval for data from long format.
}
\description{
  Computes 95 percent confidence intervals for within-subjects data in long format
  and plots the data.
  
}
\usage{
lineplot.ci(data, dv, iv, id=1, group=NULL, x=1, off=0, Bakeman=T, 
upper=T, lower=T, ylim=c(0,1), xlim=NULL, pt=15:25, 
col=rep("black", 11), ptcol=rep("white",11),na.rm=T, cex=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  The data to plot. Must be in long format.
}
\item{dv}{Column (name or number) of dependent variable.}
\item{iv}{Vector of columns of the independent variable (numbers or names): a maximum of 2!}
\item{id}{Column containing id (number or name; default: 1).}
\item{group}{Column containing between-subjects variable (number of name).}
\item{x}{Number of column within iv that is used as the x-axis (by default the first column in iv).}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a line plot with within-subjects 95 percent confidence intervals 
  computed with Bakeman & McArthur correction.
}
\references{
  
  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\author{
  Dr. Prof. K. Oberauer
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
