\name{nonEnglishNames}
\docType{data}
\alias{nonEnglishNames}
\title{
  Names with Character Set Problems
}
\description{
  A \code{\link{data.frame}} describing names containing character codes
  rare or non-existent in standard English text, e.g., with various
  accent marks that may not be coded consistenty in different locales or
  by different software.
}
\usage{data(nonEnglishNames)}
\format{
  A \code{data.frame} with two columns:
  \describe{
    \item{nonEnglish}{
      a character vector containing names that often have non-standard
      characters with the non-standard characters replaced by "_"
    }
    \item{English}{
      a character vector containing a standard English-character
      translation of \code{nonEnglish}
    }
  }
}
%details{}
%\source{}
%\references{}
\seealso{
  \code{\link{grepNonStandardCharacters}},
  \code{\link{subNonStandardCharacters}}
}
\examples{
data(nonEnglishNames)

\dontshow{stopifnot(}
all.equal(dim(nonEnglishNames), c(17, 2))
\dontshow{)}

}
\keyword{datasets}
