\name{Ping}
\alias{Ping}
\title{
  ping a Uniform resource locator (URL)
}
\description{
  ping a Uniform resource locator (URL) or Internet Protocol (IP)
  address.
}
\usage{
Ping(url, pingArgs='', ...)
}
\arguments{
  \item{url}{
    a character string of a URL or IP address to ping
  }
  \item{pingArgs}{
    arguments to pass to the \code{ping} command of typical operating
    systems via

    pingResult <- system(paste('ping', pingArgs, url), intern=TRUE, ...)
  }
  \item{\dots}{
    optional arguments for \code{\link{system}}.
  }
}
\details{
    system(paste('ping', pingArgs, url), intern=TRUE, ...)
}
\value{
  list with the following components:

  \item{rawResults}{
    character vector of the raw results from the ping command
  }
  \item{rawNumbers}{
    numeric vector of the times measured
  }
  \item{counts}{
    numeric vector of numbers of packets sent, received, and lost
  }
  \item{p.lost}{
    proportion lost = lost / sent
  }
  \item{stats}{
    numeric vector of min, avg (mean), max, and mdev
    (standard deviation) of the measured round trip times
  }
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{system}}
}
%\references{}
\examples{
##
## good
##
(google <- Ping('google.com'))

\dontshow{stopifnot(}
with(google, (counts[1]>0) && (counts[3]<1))
\dontshow{)}

##
## obviously impossible
##
(couldnotfindhost <- Ping('oops'))

\dontshow{stopifnot(}
with(couldnotfindhost,
     length(grep('could not find host', rawResults))>0)
\dontshow{)}

##
## impossible, but not so obvious
##
(requesttimedout <- Ping('requesttimedout.com'))

\dontshow{stopifnot(}
with(requesttimedout, (counts[1]>0) && (counts[2]<1) &&
         (counts[3]>0))
\dontshow{)}

}
\keyword{IO}
