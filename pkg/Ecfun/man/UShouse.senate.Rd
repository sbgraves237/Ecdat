\name{UShouse.senate}
\alias{UShouse.senate}
\title{
  Create a list of members of the US House and Senate
}
\description{
  Combine the output of \code{\link{readUShouse}} and
  \code{\link{readUSsenate}}.
}
\usage{
UShouse.senate(house=readUShouse(), senate=readUSsenate())
}
\arguments{
  \item{house, senate}{
    \code{\link{data.frame}}s as returned by the functions
    \code{\link{readUShouse}} and \code{\link{readUSsenate}},
    respectively.
  }
}
\details{
  Convert the two into a common format and rbind.
}
%\source{ }
\value{
  a \code{\link{data.frame}} with the following columns:

  \item{Office}{
    A factor identifying "House" vs. "Senate", indicating whether the
    person is in the US House or Senate
  }
  \item{state}{
    A factor identifying the state using the USPS 2-letter state code
    (all caps)
  }
  \item{district}{
    "0" or "At-Large" for members of the US House representing an entire
    state or integers in character format indicating the district.  For
    the Senate, this contains the "class", which codes the year of the
    next election for that seat is an integer multiple of 6 years after
    2012, 2008, or 2010 for class "1", "2", or "3", respectively.
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative,
    e.g., 'Democratic', 'Republican', 'Democratic-Farmer-Labor',
    'Independent'.
  }
  \item{surname}{
    family name
  }
  \item{givenname}{
    first name with possibly a middle name, nickname, and suffix (e.g.,
    Jr., III).
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{readUShouse}}
  \code{\link{readUSsenate}}
}
\examples{
house <- readUShouse()

USreps <- UShouse.senate(house)
}
\keyword{datasets}
\keyword{IO}