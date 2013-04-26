\name{readUShouse}
\alias{readUShouse}
\alias{readUShouse.gov}
\alias{readUShouse.wiki}
\title{
  Read the list of representatives in the United States House of
  Representatives
}
\description{
  Read the list of representatives in the United States House of
  Representatives.  \code{readUShouse.gov} obtains this from
  \url{house.gov}.  \code{readUShouse.wiki} gets this from the Wikipedia
  article on the "List of current members of the United States House of
  Representatives by age".  \code{readUShouse} merges the results from
  \code{readUShouse.gov} and \code{readUShouse.wiki}.
}
\usage{
readUShouse(url.gov="http://house.gov/representatives/",
            url.wiki="http://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives_by_age")
readUShouse.gov(url="http://house.gov/representatives/",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') )
readUShouse.wiki(url="http://en.wikipedia.org/wiki/List_of_current_members_of_the_United_States_House_of_Representatives_by_age",
   nonvoting=c('American Samoa', 'District of Columbia',
               'Guam', 'Northern Mariana Islands', 'Puerto Rico',
               'Virgin Islands') )
}
\arguments{
  \item{url, url.gov, url.wiki}{
    Universal resource locator to be read and processed to obtain the
    desired list
  }
  \item{nonvoting}{
    Character vector of the names of US territories that send a
    nonvoting delegate to the US House.
  }
}
\details{
  \code{readUShouse.wiki} is a wrapper for \code{\link[XML]{readHTMLTable}}.

  \code{readUShouse.gov} works as follows:

  1.  \code{House.gov <- readHTMLTable(url)}.  This is a list of 80
  tables.  The first 56 are for the 50 states and 6 territories.  The
  remaining 24 are for the first letter of the last name of the
  representatives.

  2.  Use \code{\link{rbind}} to collapse these 80 tables to 2.

  3.  Extract \code{State} from the \code{District} in the second of
  the combined tables and prepend that as a first column of the first
  table.  Add a \code{nonvoting} column for those "States" in
  \code{nonvoting}.

  \code{readUShouse} calls \code{readUShouse.gov} then
  \code{readUShouse.wiki} and merges the two.  If they are different
  (apart from the nonvoting representatives), the differences are noted
  in a warning.
}
%\source{ }
\value{
  \code{readUShouse} returns a \code{data.frame} with the following
  columns:

  \item{state}{
    A factor identifying the state person represents
  }
  \item{district}{
    the integer number of the district each person represents
  }
  \item{familyName}{
    A character vector giving the surname or family name of each
    representative
  }
  \item{givenName}{
    A character vector containing the given name of each representative
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative
  }
  \item{committees}{
    a character vector giving the committee assignments of each
    representative
  }

  \code{readUShouse.gov} returns a \code{data.frame} with the following
  columns:

  \item{State}{
    A factor identifying the state person represents
  }
  \item{District}{
    the character vector identifying the district each person
    represents.  This is either an integer in character format or "At
    Large".
  }
  \item{Name}{
    A character vector giving the name of each representative
  }
  \item{Party}{
    a factor identifying the party affiliation of each representative
    ("D" or "R").
  }
  \item{Committees}{
    a character vector giving the committee assignments of each
    representative
  }

  \code{redUShouse.wiki} returns a \code{data.frame} with all the
  information returned by \code{readUShouse} except for \code{Party} --
  unless the number of fields by row is not constant.  In the latter
  case, \code{readUShouse.gov} returns a \code{list} containing the
  information read.
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
}
\examples{
##
## house.gov
##
UShouse.gov <- readUShouse.gov()
##
## house.wiki
##
UShouse.wiki <- readUShouse.wiki()
##
## house
##
UShouse<- readUShouse()
}
\keyword{datasets}
\keyword{IO}