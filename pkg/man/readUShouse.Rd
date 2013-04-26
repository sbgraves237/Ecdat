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
readUShouse(url="http://house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') )
}
\arguments{
  \item{url}{
    Universal resource locator to be read and processed to obtain the
    desired list
  }
  \item{nonvoting}{
    Character vector of the names of US territories that send a
    nonvoting delegate to the US House.
  }
}
\details{
  1.  \code{House.gov <- readHTMLTable(url)}.  This is a list of 80
  tables.  The first 56 are for the 50 states and 6 territories.  The
  remaining 24 are for the first letter of the last name of the
  representatives.

  2.  Use \code{\link{rbind}} to collapse these 80 tables to 2.

  3.  Extract \code{State} from the \code{District} in the second of
  the combined tables and prepend that as a first column of the first
  table.  Add a \code{nonvoting} column for those "States" in
  \code{nonvoting}.
}
%\source{ }
\value{
  \code{readUShouse} returns a \code{data.frame} with the following
  columns:

  \item{State}{
    A factor identifying the state or territory the person represents
  }
  \item{state}{
    2-letter US Postal Service abbreviation for the state or territory
  }
  \item{District}{
    the character vector identifying the district each person
    represents.  This is either an integer in character format or "At
    Large".
  }
  \item{Name}{
    A character vector giving the name of each representative (in surname,
    given name format)
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative
    ("D" or "R").
  }
  \item{Room}{
    character vector identifying the room number of the office
  }
  \item{Phone}{
    character vector giving the phone number
  }
  \item{Committees}{
    a character vector giving the committee assignments of each
    representative
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
}
\examples{
UShouse <- readUShouse()
}
\keyword{datasets}
\keyword{IO}