\name{UShouse.senate}
\alias{UShouse.senate}
\title{
  Create a list of members of the US House and Senate
}
\description{
NOTE:  THIS FUNCTION IS CURRENTLY UNAVAILABLE: A CALL TO 
"RCurl::getURL" THAT RETURNED INFO ON ALL MEMBERS OF THE 
US HOUSE IN 2013 AND 2016 ONLY RETURNS 222 CHARACTERS 
AS OF 2018-01-11.  

  If and when a fix for this is found, this function may again:

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

  \item{Chamber}{
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
  \item{party}{
    a factor identifying the party affiliation of each representative,
    e.g., 'D', 'R', 'I'.
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
if(FALSE){
#NOTE:  THIS FUNCTION IS CURRENTLY UNAVAILABLE: A CALL TO 
#"RCurl::getURL" THAT RETURNED INFO ON ALL MEMBERS OF THE 
#US HOUSE IN 2013 AND 2016 ONLY RETURNS 222 CHARACTERS 
#AS OF 2018-01-11.  

#  If and when a fix for this is found, this function may again 
#  work as it did 2013-2016.

if(!fda::CRAN()){
house <- readUShouse()

USreps <- UShouse.senate(house)
}}
}
\keyword{datasets}
\keyword{IO}