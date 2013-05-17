\name{nonEnglishNames}
\docType{data}
\alias{nonEnglishNames}
\title{
  Names with Character Set Problems
}
\description{
  A \code{\link{data.frame}} describeing of names containing character
  codes rare or non-existent in English text, e.g., with various accent
  marks that may not be coded consistenty in different locales or by
  different software.
}
\usage{data(nonEnglishNames)}
\format{
  A \code{data.frame} containing:
  \describe{
    \item{nonEnglish}{
      a character vector containing non-English characters
    }
    \item{English}{
      a character vector containing a standard English-character
      translation of \code{name.}
    }
    \item{primary}{
      logical:  Is this the primary nonEnglish representation for the
      \code{English} name?
    }
  }
}
%details{}
%\source{}
%\references{}
\seealso{
  \code{\link{grepNonEnglish}}
}
\examples{
data(nonEnglishNames)
}
\keyword{datasets}
