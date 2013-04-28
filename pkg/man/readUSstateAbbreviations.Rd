\name{readUSstateAbbreviations}
\alias{readUSstateAbbreviations}
\title{
  Read a list of abbreviations of states and territories of the United
  States
}
\description{
  Read the list of abbreviations of states and territories of the United
  states from the relevant Wikipedia article
}
\usage{
readUSstateAbbreviations(url=
"http://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations")
}
\arguments{
  \item{url}{
    Universal resource locator to be read and processed to obtain the
    desired list
  }
}
\details{
  Wrapper for \code{\link[XML]{readHTMLTable}}.
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
}
\examples{
abbreviations <- readUSstateAbbreviations()
}
\keyword{datasets}
\keyword{IO}