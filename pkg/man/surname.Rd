\name{surname}
\alias{surname}
\title{
  Parse surname from standard given and surname format
}
\description{
  Identify the presumed surname in a character string assumed to
  represent a name and return the result in a character matrix with
  "surname" followed by "givenName".
}
\usage{
surname(x)
}
\arguments{
  \item{x}{
    a character vector
  }
}
\details{
  1.  Look for commas to identify a suffix like Jr. or III;  remove to
  form x2.

  2.  split <- strsplit(x2, " ")

  3.  Take the last as the surname.  Ignore complications like the
  Spanish tradition of two surnames.

  4.  Recompose the rest with any suffix as the givenName.
}
\value{
  a character matrix with two columns:  surname and givenName
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}
}
%\references{}
\examples{
tst <- c('Joe Smith', 'Teresa Angelica Sanchez de Gomez',
         'John Brown, Jr.', 'John W. Brown, III', 'John Q. Brown, I',
         'Linda Rosa Smith-Johnson')
parsed <- surname(tst)

tst2 <- matrix(c('Smith', 'Joe', 'Gomez', 'Teresa Angelica Sanchez de',
  'Brown', 'John, Jr.', 'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa'), ncol=2, byrow=TRUE)
colnames(tst2) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(parsed, tst2)
\dontshow{)}

}
\keyword{manip}
