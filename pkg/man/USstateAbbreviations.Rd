\name{USstateAbbreviations}
\docType{data}
\alias{USstateAbbreviations}
\title{
  Standard abbreviations for states of the United States }
\description{
  \code{readUSstateAbbreviations()} as of May 20, 2013.
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
    \item{ISO.1., ANSI.2., ANSI.3., USPS.4., USCG.5., Old.GPO, AP.6.,
      Other}{
      Alternative abbreviations used per different standards.  The most
      commmonly used among these may be the 2-letter codes officially
      used by the US Postal Service (\code{USPS.4.};  the number ".4."
      refers to a footnote in
      \href{http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}{the
	Wikipedia article on "List of U.S. state abbreviations"}).
    }
  }
}
%\details{}
\source{
  \href{http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}{the
    Wikipedia article on "List of U.S. state abbreviations"}
}
%\references{}
\seealso{
  \code{\link{readUSstateAbbreviations}}
  \code{\link[tools]{showNonASCII}}
  \code{\link{grepNonStandardCharacters}}
  \code{\link{subNonStandardCharacters}}
}
\examples{
##
## to use
##
data(USstateAbbreviations)

##
## to update
##

USstateAbb2 <- readUSstateAbbreviations()

# Then remove non-ASCII characters using, e.g,
# showNonASCII, grepNonStandardCharacters,
# subNonStandardCharacters, etc.

}
\keyword{datasets}
