\name{read.testURLs}
\alias{read.testURLs}
\title{
  Read a file produced by testURLs
}
\description{
  ***NOTE:  THIS IS A PRELIMINARY VERSION OF THIS FUNCTION;
  ***NOTE:  IT MAY BE CHANGED OR REMOVED IN A FUTURE RELEASE.

  \code{read.table(file.)} and return the result as an object
  of class c('testURLs', 'data.frame').
}
\usage{
read.testURLs(file.='testURLresults.csv', ...)
}
\arguments{
  \item{file.}{
    Name of a CSV file to read
  }
  \item{\dots}{ optional arguments for \code{\link{read.csv}}.  }
}
\details{
  dat <- read.csv(file., ...)

  class(dat) <- c('testURLsFile', 'data.frame')
}
\value{
  a \code{\link{data.frame}} from the file written by
  \code{\link{testURLs}}, of the same format as the \code{testResults}
  attribute of the \code{testURLs} object returned by
  \code{\link{testURLs}}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{read.csv}}
}
%\references{}
\examples{
# Test only 2 web sites, not the default 4,
# and test only twice, not the default 10 times:
tst <- testURLs(c(
 PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov/representatives"),
    n=2, maxFail=2)

# The above should have created a file 'testURLresults.csv'
# in the working directory.  Read it.

dat <- read.testURLs()

}
\keyword{IO}
