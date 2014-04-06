\name{testURLs}
\alias{testURLs}
\title{
  Test URLs for intermittent download problems
}
\description{
  ***NOTE:  THIS IS A PRELIMINARY VERSION OF THIS FUNCTION;
  ***NOTE:  IT MAY BE CHANGED OR REMOVED IN A FUTURE RELEASE.

  \code{try(getURL(...))} to read each element of \code{urls}.  After
  each try, write a row to \code{file.} indicating which of \code{urls}
  was tested, the test time in seconds, and any error message.  Repeat
  any failures up to \code{maxFail} times.  After testing each element
  of \code{urls} once, repeat \code{n} times.

  If(ping), preceed each test with "ping url[i]".  NOTE:  Some Internet
  Service Providers seem to block some attepts to use "ping" or return
  fraudulet replies to "ping".  It is included in the code, because it
  seemed like an obvious test.  However, it is not executed by default
  because the results do not necessarily reflect what people might
  expect from "ping".

  Return a list of the last successful version read if any from each
  element of \code{urls} with two attributes:  (1) "urls" containing the
  \code{urls} argument.  (2) "testResults" being an object of class
  \code{c('testURLs', 'data.frame')} of the test results written to
  \code{file.}.

  This function was written to diagnose a download problem with a
  particular Internet Service Provider (ISP).  For other tools for
  testing an ISP, see
  \href{http://www.measurementlab.net/}{measurementlab.net} or the "Test
  your ISP" software discussion by the Electronic Frontier Foundation at
  the URL mentioned in references below.
}
\usage{
testURLs(urls=c(
 wiki="http://en.wikipedia.org",
 wiki.PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov",
 house.reps="http://house.gov/representatives"),
         file.='testURLresults.csv',
         n=10, maxFail=10, warn=-1, tzone='GMT', ping=FALSE, ...)
}
\arguments{
  \item{urls}{
    a character vector assumed to be universal resource locators to pass
    to \code{\link[RCurl]{getURL}} for testing.

    The default was selected to provide a 2 x 2 experiment with two
    different web sites (en.wikipedia.org and house.gov) vs. the landing
    page and a subordinate page for each site.
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
    max tests for a continually failing URL.  This is designed to make
    it relatively easy to determine determine dependencies between
    failures.  If the failure rate is constant, the number of
    consecutive failures will follow a Poisson distribution.  Otherwise,
    it may be possible to evaluate various effects using, e.g., state
    space techniques for non-normal time series.  This could include
    daily and weekly cycles possibly with holiday effects and trends as
    well as drifts suggesting abnormal drifts in web traffic
    congestion.
  }
  \item{warn}{\code{warn} argument to pass to \code{\link{Ping}}.}
  \item{tzone}{
    Time zone for \code{Time}.  Defaults to GMT (UTC).  tzone=NULL will
    use the current locale.
  }
  \item{ping}{
    logical:  TRUE to include \code{\link{Ping}}, FALSE otherwise.
  }
  \item{\dots}{ optional arguments for \code{\link{Ping}}. }
}
\details{
  for(i in 1:n):

  1.  pingi <- Ping(urls[i], ...)

  2.  The time for each call to \code{\link[RCurl]{getURL}} is computed
  by computing \code{start.time <- proc.time()} before calling
  \code{try(getURL(.))}, then computing the following after:

  \code{elapsed.time <- max(proc.time() - start.time, na.rm=TRUE)}

  After each of the \code{urls} is tested, a summary of the results is
  appended to \code{file.}.  This includes the pingi[['stats']],
  \code{elapsed.time} and the error message if the download failed.

  The Electronic Frontier Foundation provides a table of existing
  software to "Test your ISP";  see the references below.  This table
  includes a column noting whether the software is "active" (sending
  test traffic) or "passive" (observing the way the network treats
  natural traffic).  The current \code{testURLs} function is "active",
  because it asks for a copy of the code at the indicated URL.
}
\value{
  an object of class \code{testURLs}, which in this case is a list of
  the last successful result returned by \code{\link[RCurl]{getURL}} for
  each element of \code{urls} with the following attributes:

  \item{urls}{ the \code{urls} argument used for this call }
  \item{testURLresults}{
    an object of class \code{c('testURLs', 'data.frame')} of the
    data written to \code{file.}.  This has the following columns:

    \itemize{
      \item{Time}{
	\code{date()} for the time a particular test started
      }
      \item{URL}{ the name in \code{urls} of the URL tested}
      \item{ping statistics}{
	several columns with the \code{count} and \code{stats} returned
	by \code{\link{Ping}}.
      }
      \item{readTime}{
	time in seconds for the attempt to read the URL (getURL(urls[j]))
	to complete.
      }
      \item{error}{
	character:  '' if the read attempt was successful;  the error
	message if not.
      }
    }
  }
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{try}}
  \code{\link[RCurl]{getURL}}
  \code{Ping}
}
\references{
  \href{http://www.measurementlab.net/}{measurementlab.net}
  \href{https://www.eff.org/testyourisp}{"Test your ISP" software
    discussion by the Electronic Frontier Foundation}
  \href{https://www.eff.org/deeplinks/2008/03/keeping-isps-honest}{
    "active" (sending test traffic) or "passive" (observing the way the network
    treats natural traffic).}
}
\examples{
# Test only 2 web sites, not the default 4,
# and test only twice, not the default 10 times:
tst <- testURLs(c(
 PVI="http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
 house="http://house.gov/representatives"),
    n=2, maxFail=2)

\dontshow{stopifnot(}
(class(tst) == 'testURLs') &&
all(names(tst) == c('PVI', 'house')) &&
all(names(attributes(tst)) ==
    c('names', 'urls', 'testURLresults', 'class'))
\dontshow{)}

}
\keyword{IO}
