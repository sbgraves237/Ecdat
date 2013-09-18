\name{readCookPVI}
\alias{readCookPVI}
\title{
  Read Cook Partisan Voting Index
}
\description{
  Read tables of the Cook Partisan Voting Index and returns a list with
  components 'House' and 'Senate'.
}
\usage{
readCookPVI(url=
"http://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index",
    stateAbbreviations=USstateAbbreviations)
}
\arguments{
  \item{url}{
    Universal resource locator to be read and processed to obtain the
    desired lists.
  }
  \item{stateAbbreviations}{
    a \code{\link{data.frame}} giving names and alternative codes for US
    states and territories.  This must have a column named "Name" giving
    the names of all 50 states as they appear on the \code{url} and
    another whose name includes "USPS" giving the correspoding 2-letter
    codes.
  }
}
\details{
  The primary source for these data is the Cook Political Report web
  site.  However, the current URL we have for these data on that web
  site includes "2012" in the title.  If and when the numbers are
  updated, we would expect that file name to change.

  To avoid that problem the code is currently set to read from the
  Wikipedia article on "Cook Partisan Voting Index".

  The algorithm reads the web site into a list, finds the desired tables
  on the list, then parses and formats them as desired.
}
\value{
  A list with components "House" and "Senate".  Each contains a
  \code{\link{data.frame}}.  The "House" data.frame includes the
  following columns:

  \item{State}{name of the state}
  \item{District}{District, e.g, 1st, 2nd, At-Large}
  \item{PVInum}{
    PVI as a number ranging from roughly 50 to 150.  100 means that the
    vote split in that district was within 0.5 percent of the national
    average.  101 means that it tilts 1 percent (after rounding) to
    Republican.  98 means that it tilts 1 percent to Democratic;  99 is
    not used.
  }
  \item{PVIchar}{
    PVI rating in character format.  For example, 'D+1' means that the
    vote tilted 1 percent toward Democratic more than the national
    average.  'R+1' means that it tilted 1 percent toward Republican.
  }
  \item{PartyOfRepresentative}{
    Party of the incumbent, either 'Republican' or 'Democratic'
  }

  The 'Senate' data.frame includes the following columns:

  \item{State}{name of the state}
  \item{PVInum}{PVI numeric, as for 'House'}
  \item{PVIchar}{PVI rating in character format, as for 'House'}
  \item{PartyOfGovernor}{
    Party of the Governor of the state
  }
  \item{PartyInSenate}{
    party of the incumbent senators, either 'Republican', Democratic',
    or 'Both'.
  }
  \item{houseBalanceNum}{
    House balance as a number with 0 = 100 percent Democratic, 99.9 =
    100 percent Republican, and 500 for the same number of Republicans
    as Democrats.
  }
  \item{houseBalanceChar}{
    Count by pary in the house delegation for that state, e.g.,
    '6R, 1D' for 6 Republicans and 1 Democrat.
  }
}
\source{
  \href{https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index}{Wikipedia,
    "Cook Partisan Voting Index"}
  \href{http://cookpolitical.com/}{The Cook Political Report}
}
\author{Spencer Graves}
\examples{
CookPVI <- readCookPVI()
}
\keyword{IO}
