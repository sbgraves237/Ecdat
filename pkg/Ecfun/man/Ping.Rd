\name{Ping}
\alias{Ping}
\title{
  ping a Uniform resource locator (URL)
}
\description{
  ***NOTE:  THIS IS A PRELIMINARY VERSION OF THIS FUNCTION;
  ***NOTE:  IT MAY BE CHANGED OR REMOVED IN A FUTURE RELEASE.

  ping a Uniform resource locator (URL) or Internet Protocol (IP)
  address.

  NOTE:  Some Internet Service Providers (ISPs) play games with "ping".
  That makes the results of \code{Ping} unreliable.
}
\usage{
Ping(url, pingArgs='', warn=NA,
     show.output.on.console=FALSE)
}
\arguments{
  \item{url}{
    a character string of a URL or IP address to ping.  If \code{url} is
    a vector of length greater than 1, only the first component is
    used.
  }
  \item{pingArgs}{
    arguments to pass to the \code{ping} command of typical operating
    systems via

    pingResult <- system(paste('ping', pingArgs, url), intern=TRUE, ...)
  }
  \item{warn}{
    value for \code{options('warn')} during the call to
    \code{\link{system}}.  \code{NA} to not change
    \code{options('warn')} during this call.
  }
  \item{show.output.on.console}{
    argument for \code{\link{system}}.
  }
}
\details{
  1.  urlSplit0 <- strsplit(url, '://')[[1]]

  2.  urlS0 <- urlSplit0[min(2, length(urlSplit0))]

  3.  host <- strsplit(urlS0, '/')[[1]][1]

  4.  pingCmd <- paste('ping', pingArgs, host)

  5.  system(pingCmd, intern=TRUE, ...)
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
  \code{\link{system}}, \code{\link{options}}
}
%\references{}
\examples{
##
## Some ISPs play games with ping.
## Therefore, the results are not reliable.
##
\dontrun{
##
## good
##
(google <- Ping('http://google.com/ping works on host not pages'))

\dontshow{stopifnot(}
with(google, (counts[1]>0) && (counts[3]<1))
\dontshow{)}

##
## ping oops <<-- at one time, this failed.
##      However, with some ISPs, it works, so don't test it.
##

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
}
\keyword{IO}
