\name{readUSsenate}
\alias{readUSsenate}
\title{
  Read the list of elected officials in the United 
  States Senate
}
\description{
  Read the list of elected officials in the United 
  States Senate.
}
\usage{
readUSsenate(url.=
"https://www.senate.gov/general/contact_information/senators_cfm.xml",
    stateAbbreviations=Ecdat::USstateAbbreviations,
    fixNonStandard=subNonStandardNames, ...)
}
\arguments{
  \item{url.}{
    Universal resource locator to be read and 
    processed to obtain the desired list.  (The 
    current default was suggested by Julia Silge.)  
  }
  \item{stateAbbreviations}{
    a \code{\link{data.frame}} giving names and 
    alternative codes for US states and territories.  
    This must have a column named "Name" giving the 
    names of all 50 states as they appear on the 
    \code{url} and another whose name includes 
    "USPS" giving the correspoding 2-letter codes.
  }
  \item{fixNonStandard}{
    function to look for and repair nonstandard names 
    such as names containing characters with accent marks 
    that are sometimes mangled by different software.
  }
  \item{\dots}{ optional arguments passed to 
  \code{fixNonStandard}}
}
\details{
  1.  \code{Senate <- readHTMLTable(url)}

  2.  Use \code{\link{camelParse}} to remove duplication 
  in \code{Name}.

  3.  Look for and fix \code{surname} and \code{givenName} 
  with nonstandard characters using \code{fixNonStandard}.
}
%\source{ }
\value{
  \code{readUSsenate} returns a \code{data.frame} with the 
  following columns:

  \item{State}{
    A factor identifying the state the person represents
  }
  \item{state}{
    A factor giving the 2-letter USPS code for the state 
    represented
  }
  \item{Class}{
    an integer = 1, 2, or 3 for election in the 6-year 
    cycle including 2008, 2010, or 2012, respectively.
  }
  \item{Name}{
    A character vector giving the name of each 
    representative (in surname, given name format)
  }
  \item{party}{
    a factor identifying the party affiliation of each 
    representative ("D", "R", "I" or ...)
  }
  \item{address}{
    character vector of physical addresses of offices 
    in Washington, DC.
  }
  \item{phone}{
    character vector the phone number of the offices in 
    Washington, DC.
  }
  \item{email}{
    a character vector giving the URL of their office 
    contact form.  
  }
  \item{website}{
    a character vector giving the URL of their web site 
    in "senate.gov".  
  }
  \item{surname}{
    character vector giving the surname of each 
    representative
  }
  \item{givenName}{
    given name of each representative (possibly with 
    middle name or initial, a nickname, and a suffix like 
    "Jr.")
  }
  \item{leadership_position}{
    a character vector giving the names of the official 
    leaders or blank.  
  }
}
%\references{}
\author{Spencer Graves with help from Julia Silge}
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
if(!fda::CRAN()){
USsenate <- readUSsenate()
}
}
\keyword{IO}