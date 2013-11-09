\name{read.testURLsFile}
\alias{read.testURLsFile}
\title{
  Read a file produced by testURLs
}
\description{
  \code{read.table(file.)} and name the result of class
  c('testURLsFile', 'data.frame').
}
\usage{
read.testURLsFile(file.='testURLresults.csv', ...)
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

dat <- read.testURLsFile()

}
\keyword{IO}
