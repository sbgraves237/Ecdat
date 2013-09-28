\name{readUSsenate}
\alias{readUSsenate}
\title{
  Read the list of elected officials in the United States Senate
}
\description{
  Read the list of elected officias in the United States Senate.
}
\usage{
readUSsenate(url.=
"http://en.wikipedia.org/wiki/List_of_current_United_States_Senators",
    stateAbbreviations=USstateAbbreviations,
   fixNonStandard=subNonStandardNames, ...)
}
\arguments{
  \item{url.}{
    Universal resource locator to be read and processed to obtain the
    desired list

    NOTE:  On April 26, 2013 the obvious naive use of
    \code{\link[XML]{readHTMLTable}} worked with the Wikipedia article
    on the US Senate but did not work with "senate.gov".
  }
  \item{stateAbbreviations}{
    a \code{\link{data.frame}} giving names and alternative codes for US
    states and territories.  This must have a column named "Name" giving
    the names of all 50 states as they appear on the \code{url} and
    another whose name includes "USPS" giving the correspoding 2-letter
    codes.
  }
  \item{fixNonStandard}{
    function to look for and repair nonstandard names such as names
    containing characters with accent marks that are sometimes mangled
    by different software.
  }
  \item{\dots}{ optional arguments passed to \code{fixNonStandard}}
}
\details{
  1.  \code{Senate <- readHTMLTable(url)}

  2.  Use \code{\link{camelParse}} to remove duplication in \code{Name}.

  3.  Look for and fix \code{surname} and \code{givenName} with
  nonstandard characters using \code{fixNonStandard}.
}
%\source{ }
\value{
  \code{readUSsenate} returns a \code{data.frame} with the following
  columns:

  \item{State}{
    A factor identifying the state the person represents
  }
  \item{state}{
    A factor giving the 2-letter USPS code for the state represented
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
  \item{surname}{
    character vector giving the surname of each representative
  }
  \item{givenName}{
    given name of each representative (possibly with middle name or
    initial, a nickname, and a suffix like "Jr.")
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
  \code{\link{camelParse}} to remove duplication in \code{Name}
  \code{\link{readUShouse}}
  \code{\link{UShouse.senate}}
  \code{\link{parseName}}
  \code{\link{subNonStandardNames}}
}
\examples{
USsenate <- readUSsenate()
}
\keyword{IO}