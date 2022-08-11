\name{BenchmarksWM-package}
\alias{BenchmarksWM-package}
\alias{BenchmarksWM}
\docType{package}
\title{
\packageTitle{BenchmarksWM}
}
\description{
\packageDescription{BenchmarksWM}
}
\details{
  Go to \code{\link{benchmarksWM.overview}} for an overview of all the Benchmark subsections and datasets.

The DESCRIPTION file:
\packageDESCRIPTION{BenchmarksWM}


A collection of benchmark datasets for working memory researchers, as
identified by Oberauer et al. (2018). This package is a fork of Oberauer
(2019), with all datasets placed in a common format and documented with
help files, following the structure specified by Wills et al. (2017).

The package is likely to be useful for those wishing to conduct
computational modelling, and/or secondary data analysis, of these
benchmark datasets.

The package contains one or more datasets (each with a help file) for each
benchmark listed by Oberauer et al. (2018). For a
complete list of included datasets, use \code{library(help =
"BenchmarksWM")}. For a more structured view of all datasets organized by topics,
use \code{?benchmarksWM.overview} or \code{help(benchmarksWM.overview)}.

 }
\author{
\packageAuthor{BenchmarksWM}

Maintainer: \packageMaintainer{BenchmarksWM}
}
\references{

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

  Wills, A.J., O'Connell, G., Edmunds, C.E.R. & Inkster, A.B. (2017).
  Progress in modeling through distributed collaboration: Concepts,
  tools, and category-learning examples. \emph{The Psychology of
  Learning and Motivation, 66}, 79-115.

}
\keyword{ package }
\examples{

data(unsworth06a)

}
