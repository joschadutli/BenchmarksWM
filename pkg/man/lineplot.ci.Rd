\name{lineplot.ci}
\alias{lineplot.ci}
\title{
  Plotting lines with confidence interval for data from long format.
}
\description{
  Computes 95 percent confidence intervals for within-subjects data in long format
  and plots the data.
  
}
\usage{
lineplot.ci(data, dv, iv, id = 1, group = NULL, x = 1, off = 0, Bakeman = TRUE, 
upper = TRUE, lower = TRUE, ylim = c(0,1), xlim = NULL, pt = 15:25, 
col = rep("black", 11), ptcol = rep("white",11), na.rm = TRUE, cex = 1, ...)
}

\arguments{
  \item{data}{ The data to plot. Must be in long format.  }
  \item{dv}{Column (name or number) of dependent variable.}
  \item{iv}{Vector of columns of the independent variable (numbers or
    names): a maximum of 2!}
  \item{id}{Column containing id (number or name; default: 1).}
  \item{group}{Column containing between-subjects variable (number of
    name).}
  \item{x}{Number of column within iv that is used as the x-axis (by
    default the first column in iv).}
  \item{off}{x-axis offset for the 95\% confidence intervals.}
  \item{Bakeman}{Calculate Bakeman & McArthur (1996) correction}

\item{upper}{Unused}

\item{lower}{Unused}

\item{ylim}{Limits on y-axis, passed to base R \code{plot} function}

\item{xlim}{Limits on x-axis, passed to base R \code{plot} function}

\item{pt}{Plot symbols, passed to pch argument of base R \code{plot}
function}

\item{col}{Foreground colour, passed to col argument of base R
  \code{plot} function}

\item{ptcol}{Background colour, passed to bg argument of base R
  \code{plot} function }

\item{na.rm}{Remove NAs from dataset}

\item{cex}{Symbol size, passed to cex argument of base R
  \code{plot} function }

}
\details{
  Returns a line plot with within-subjects 95 percent confidence intervals 
  computed with Bakeman & McArthur correction.
}

\references{

  Bakeman, R., & McArthur, D. (1996). Picturing repeated measures:
  Comments on Loftus, Morrison, and others. \emph{Behavior Research
  Methods, Instruments, & Computers, 28}, 584-589. doi:10.3758/BF03200546

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\author{
  Dr. Prof. K. Oberauer
}
