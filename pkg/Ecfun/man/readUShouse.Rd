\name{readUShouse}
\alias{readUShouse}
\title{
  Read the list of representatives in the United States House of
  Representatives
}
\description{
  Read the list of representatives in the United States House of
  Representatives.
}
\usage{
readUShouse(url.="http://www.house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands'),
   fixNonStandard=subNonStandardNames, ...)
}
\arguments{
  \item{url.}{
    Universal resource locator to be read and processed to obtain
    the desired list
  }
  \item{nonvoting}{
    Character vector of the names of US territories that send a
    nonvoting delegate to the US House.
  }
  \item{fixNonStandard}{
    function to look for and repair nonstandard names such as names
    containing characters with accent marks that are sometimes
    mangled by different software.  Use \code{\link{identity}} if
    this is not desired.
  }
  \item{\dots}{ optional arguments passed to \code{fixNonStandard}}
}
\details{
  1.  \code{House.gov <- readHTMLTable(url)}.  As of April 2013,
  this is a list of 80 tables.  The first 56 are for the 50 states 
  and 6 territories.  The remaining 24 are for the first letter of 
  the last name of the representatives.

  2.  Use \code{\link{rbind}} to collapse these into 2 tables.  The
  first has the district as a number without identifying the state
  (because that was with the names of the first 56 tables in
  House.gov).  The second has the state names but with the district
  numbers in a form not easily parsed.

  3.  Obtain the state names from the second table to match the
  names of the representatives in the first.

  4.  Add a \code{nonvoting} column for those "States" in
  \code{nonvoting}.

  5.  Look for and fix \code{surname} and \code{givenName} with
  nonstandard characters using \code{fixNonStandard}.
}
%\source{ }
\value{
  \code{readUShouse} returns a \code{data.frame} with row names 
  = District (State + district, like "New York 3rd") and the 
  following columns:  

  \item{State}{
    A \code{\link{factor}} identifying the state or territory 
    the person represents
  }
  \item{state}{
    A \code{\link{factor}} giving the 2-letter US Postal Service 
    abbreviation for the state or non-state governmental entity. 
  }
  \item{district}{
    An \code{\link{integer}} identifying the district each person 
    represents, with 0 for entities with only one representative, 
    designated "At Large" for states, "Resident Commissioner" for 
    Puerto Rico, and "Delegate" for the territories.  
  }
  \item{Name}{
    A \code{\link{character}} vector giving the name of each 
    representative (in "surname, given name" format)
  }
  \item{party}{
    a \code{\link{factor}} identifying the party affiliation of 
    each representative ("D" or "R").
  }
  \item{Room}{
    a \code{\link{character}} vector identifying the room number 
    of the office
  }
  \item{Phone}{
    a \code{\link{character}} vector giving the phone number
  }
  \item{Committees}{
    a \code{\link{character}} vector giving the committee 
    assignments of each representative
  }
  \item{surname}{
    a \code{\link{character}} vector giving the surname of each 
    representative
  }
  \item{givenName}{
    a \code{\link{character}} vector with the given name of 
    each representative (possibly with middle name or initial, 
    a nickname, and a suffix like "Jr.")
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
  \code{\link{readUSsenate}}
  \code{\link{UShouse.senate}}
  \code{\link{parseName}}
  \code{\link{readUSstateAbbreviations}}
  \code{\link{subNonStandardNames}}
  \code{\link{readCookPVI}}
}
\examples{
if(!fda::CRAN()){
UShouse <- readUShouse()
}
}
\keyword{IO}