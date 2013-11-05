\name{testURLs}
\alias{testURLs}
\title{
  Test URLs for intermittent download problems
}
\description{
  \code{try(getURL(...))} \code{n} times to read each element of
  \code{urls}.  After each try, write a row to \code{file.} indicating
  which of \code{urls} was tested, the \code{ping} time before the test,
  the test time, and any error message.  Each failure is repeated up to
  \code{maxFail} times.

  Return a list of the last successful version read if any from each
  element of \code{urls} with two attributes:  (1) "urls" containing
  the \code{urls} argument.  (2) "testResults" being a \code{data.frame}
  of the test results written to \code{file.}.

  This function was written to diagnose a download problem with a
  particular Internet Service Provider (ISP).  For other tools for
  testing an ISP, see \href{https://www.eff.org/testyourisp}{"Test your
  ISP" software discussion by the Electronic Frontier Foundation}.
}
\usage{
testURLs(urls=c(
 wiki="http://en.wikipedia.org",
 wiki.PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov",
 house.reps="http://house.gov/representatives"),
         file.='testURLresults.csv',
         n=10, maxFail=10)
}
\arguments{
  \item{urls}{
    a character vector assumed to be universal resource locators to pass
    to \code{\link[RCurl]{getURL}} for testing.
  }
  \item{file.}{
    Name of a CSV file to which to write the results.  If the file
    already exists, new results are appended to it.
  }
  \item{n}{
    number of times to repeat the cycle testing each member of
    \code{urls}.
  }
  \item{maxFail}{
    max tests for a continually failing URL
  }
}
\details{
  1.

  The time for each call to \code{\link[RCurl]{getURL}} is computed by
  computing \code{start.time <- proc.time()} before calling
  \code{try(getURL(.))}, then computing the following after:

  \code{elapsed.time <- max(proc.time() - start.time, na.rm=TRUE)}

  After each iteration of testing the \code{urls}, append summary
  statistics for that iteration to \code{file.}.  After all tests are
  completed, append tables of the error messaged if any to that
  \code{file.}.
}
\value{
  a list of the last successful result returned by
  \code{\link[RCurl]{getURL}} for each element of \code{urls} with the
  following attributes:

  \item{urls}{ the \code{urls} argument used for this call }
  \item{testResults}{
    a numeric matrix of the success (1) or failure (0) and the
    \code{elapsed.time} for each \code{\link{try}}.  This matrix
    contains the same data already written to \code{file.}.
  }
  \item{errorMessages}{
    a list of tables of the error messages returned, if any
  }
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{try}}
  \code{\link[RCurl]{getURL}}
}
%\references{}
\examples{
# Test only 2 web sites, not the default 4,
# and test only once, not the default 10 times:
tst <- testURLs(c(
 PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov/representatives"),
    n=1)
}
\keyword{IO}
