\name{as.Date1970}
\alias{as.Date1970}
\title{
  Date from a number of days since the start of 1970.
}
\description{
  \code{as.Date.numeric} requires orgin to be specified.  The present
  function assumes that this origin is January 1, 1970.
}
\usage{
as.Date1970(x, ...)
}
\arguments{
  \item{x}{
    a numeric vector of dates in days since the start of 1970.
  }
  \item{\dots}{
    optional arguments to pass to as.Date.
  }
}
%\details{}
\value{
  Returns a vector of Dates
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{as.Date}}
  \code{\link[fda]{as.POSIXct1970}}
}
%\references{}
\examples{
days <- c(0, 1, 365)
Dates <- as.Date1970(days)

\dontshow{stopifnot(}
all.equal(c('1970-01-01', '1970-01-02', '1971-01-01'),
          as.character(Dates))
\dontshow{)}

\dontshow{stopifnot(}
all.equal(days, as.numeric(Dates))
\dontshow{)}

}
\keyword{manip}
