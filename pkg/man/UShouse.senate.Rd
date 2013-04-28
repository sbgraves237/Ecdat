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

  \item{houseSenate}{
    A factor identifying "Rep" vs. "Sen", indicating whether the person
    is in the US House or Senate
  }
  \item{state}{
    A factor identifying the state using the USPTO 2-letter state code
  }
  \item{District}{
    "At-Large" for members of the US House representing an entire state
    or integers in character format indicating the district for the US
    House of the class (6-year election year cycle, "1" includes 2012,
    "2" includes 2008, "3" includes 2010) for the Senate.
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative
    ("D", "R", or "I" for "Democrat", "Republican" or "Independent").
  }
  \item{surname}{
    family name
  }
  \item{givenname}{
    first name with possible a middle name, nickname, and suffix (e.g.,
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
USreps <- UShouse.senate()
}
\keyword{datasets}
\keyword{IO}