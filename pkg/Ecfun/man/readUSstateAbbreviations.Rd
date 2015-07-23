\name{readUSstateAbbreviations}
\alias{readUSstateAbbreviations}
\title{
  Read a list of abbreviations of states and territories of the
  United States
}
\description{
  Read the list of abbreviations of states and territories of the 
  United States from the relevant Wikipedia article
}
\usage{
readUSstateAbbreviations(url.=
"https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations",
      clean=TRUE, Names=c('Name', 'Status', 'ISO', 'ANSI.letters',
          'ANSI.digits', 'USPS', 'USCG', 'Old.GPO', 'AP', 'Other') )
}
\arguments{
  \item{url.}{
    Universal resource locator to be read and processed to obtain the
    desired list
  }
  \item{clean}{
    logical:  If TRUE, clean the data using
    \code{\link{subNonStandardCharacters}} and \code{strsplit(x, "\\[")}
  }
  \item{Names}{
    names for the columns of the data matrix read;  ignored with a
    warning if the lengths do not match
  }
}
\details{
  Wrapper for \code{\link[XML]{readHTMLTable}}.

  NOTE:  \code{readHTMLTable(url)} returns a list of length 7, only one
  of which is the table we want.  Moreover, that table contains some
  duplicates, which are removed by \code{readUSstateAbbreviations}.  For
  example, 'NB' is an "Obsolete postal code" for Nebraska.  If you need
  this, please consult the Wikipedia article.
}
%\source{ }
\value{
  \code{readUSstateAbbreviatons} returns a \code{data.frame} from the
  table in
  \href{http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations}{the
    Wikipedia article on "List of U.S. state abbreviations"}.
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link[RCurl]{getURL}}
  \code{\link[XML]{readHTMLTable}}
  \code{\link{make.names}}
  \code{\link[Ecdat]{USstateAbbreviations}}
}
\examples{
if(!fda::CRAN()){
abbreviations <- readUSstateAbbreviations()
}
}
\keyword{datasets}
\keyword{IO}