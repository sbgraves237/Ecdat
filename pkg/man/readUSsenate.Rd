\name{readUSsenate}
\alias{readUSsenate}
\title{
  Read the list of elected officials in the United States Senate
}
\description{
  Read the list of elected officias in the United States Senate.
}
\usage{
readUSsenate(url=
"http://en.wikipedia.org/wiki/List_of_current_United_States_Senators")
}
\arguments{
  \item{url}{
    Universal resource locator to be read and processed to obtain the
    desired list

    NOTE:  On April 26, 2013 the obvious naive use of
    \code{\link[XML]{readHTMLTable}} worked with the Wikipedia article
    on the US Senate but did not work with "senate.gov".
  }
}
\details{
  \code{Senate <- readHTMLTable(url)}.
}
%\source{ }
\value{
  \code{readUSsenate} returns a \code{data.frame} with the following
  columns:

  \item{State}{
    A factor identifying the state or territory the person represents
  }
  \item{Class}{
    "1", "2", or "3" for election in the 6-year cycle including 2008,
    2010, or 2012, respectively.
  }
  \item{Name}{
    A character vector giving the name of each representative (in surname,
    given name format)
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative
    ("Democrat", "Republican", or "Independent").
  }
  \item{Experience}{
    character vector highlighting prior experience.
  }
  \item{assumedOffice}{
    character vector giving the date assumed office
  }
  \item{Born}{
    a character vector giving the year of birth
  }
  \item{endOffice}{
    a character vector giving the last day in the present term.
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
}
\examples{
USsenate <- readUSsenate()
}
\keyword{datasets}
\keyword{IO}