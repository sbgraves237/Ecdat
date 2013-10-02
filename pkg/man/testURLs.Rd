\name{testURLs}
\alias{testURLs}
\title{
  Test URLs for intermittent download problems
}
\description{
  \code{try(getURL(...))} \code{n} times to read each element of
  \code{urls}.  After each try, write a row to \code{file} indicating if
  \code{\link[RCurl]{getURL}} was successful (1) or not (0) and the time
  taken by each \code{\link[RCurl]{getURL}} call.  Return a list of the
  last successful version read if any from each element of \code{urls}
  with attributes "urls" containning the \code{urls} argument and
  "testResults" containing a matrix of the success indicators and times
  for each element of \code{urls}.

  This function was written to diagnose a download problem with a
  particular Internet Service Provider.
}
\usage{
testURLs(urls=c(
 PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov/representatives",
 senate=
  "http://en.wikipedia.org/wiki/List_of_current_United_States_Senators",
 abbr="http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"),
         file.='testURLresults.csv',
         n=10)
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
}
\details{
  The time for each call to \code{\link[RCurl]{getURL}} is computed by
  computing \code{start.time <- proc.time()} before calling
  \code{try(getURL(.))}, then computing the following after:

  \code{elapsed.time <- max(proc.time() - start.time, na.rm=TRUE)}
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
# Test 2 web sites, not the default 4:
tst <- testURLs(c(
 PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov/representatives"),
    n=1)
}
\keyword{IO}
