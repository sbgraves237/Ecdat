\name{USstateAbbreviations}
\docType{data}
\alias{USstateAbbreviations}
\title{
  Standard abbreviations for states of the United States }
\description{
  The object returned by \code{Ecfun::readUSstateAbbreviations()} on May 20,
  2013.
}
\usage{
data(USstateAbbreviations)}
\format{
  A \code{data.frame} containing 10 different character vectors of names
  or codes for 76 different political entities including the United
  States, the 50 states within the US, plus the District of Columbia, US
  territories and other political designation, some of which are
  obsolete but are included for historical reference.
  \describe{
    \item{Name}{ The standard name of the entity. }
    \item{Status}{
      description of status, e.g., state / commonwealth vs. island,
      territory, military mail code, etc.
    }
    \item{ISO, ANSI.letters, ANSI.digits, USPS, USCG, Old.GPO, AP,
      Other}{
      Alternative abbreviations used per different standards.  The most
      commonly used among these may be the 2-letter codes officially
      used by the US Postal Service (\code{USPS}).
    }
  }
}
\details{
  This was read from
  \href{https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}{the
    Wikipedia article on "List of U.S. state abbreviations"}
}
\source{
  \href{https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}{the
    Wikipedia article on "List of U.S. state abbreviations"}
}
%\references{}
\seealso{
  \code{\link[Ecfun]{readUSstateAbbreviations}}
  \code{\link[tools]{showNonASCII}}
  \code{\link[Ecfun]{grepNonStandardCharacters}}
  \code{\link[Ecfun]{subNonStandardCharacters}}
}
\examples{
##
## to use
##
data(USstateAbbreviations)

##
## to update
##
\dontrun{
USstateAbb2 <- readUSstateAbbreviations()
}
}
\keyword{datasets}
