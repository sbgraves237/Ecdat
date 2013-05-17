\name{parseName}
\alias{parseName}
\title{
  Parse surname and given name
}
\description{
  Identify the presumed surname in a character string assumed to
  represent a name and return the result in a character matrix with
  "surname" followed by "givenName".
}
\usage{
parseName(x, surnameFirst=FALSE,
          suffix=c('Jr.', 'I', 'II', 'III', 'IV', 'Sr.') )
}
\arguments{
  \item{x}{
    a character vector
  }
  \item{surnameFirst}{
    logical:  If TRUE, the surname comes first followed by a comma
    (","), then the given name.  Otherwise, parse the surname from a
    standard Western "John Smith, Jr." format.
  }
  \item{suffix}{
    character vector of strings that are NOT a surname but might appear
    at the end without a comma that would otherwise identify it as a
    suffix.
  }
}
\details{
  If \code{surnameFirst} is \code{FALSE}:

  1.  Look for commas to identify a suffix like Jr. or III;  remove and
  call the rest x2.

  2.  split <- strsplit(x2, " ")

  3.  Take the last as the surname.

  4.  If the "surname" found per 3 is in \code{suffix}, save to append
  it to the \code{givenName} and recurse to get the actual surname.

  NOTE:  This gives the wrong answer with double surnames written
  without a hyphen in the Spanish tradition, in which, e.g., "Anistasio
  Somoza Debayle", "Somoza Debayle" give the (first) surnames of
  Anistasio's father and mother, respectively:  The current algorithm
  would return "Debayle" as the surname, which is incorrect.

  5.  Recompose the rest with any suffix as the givenName.
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
##
## 1.  Parse standard first-last name format
##
tst <- c('Joe Smith', 'Teresa Angelica Sanchez de Gomez',
         'John Brown, Jr.', 'John W. Brown III', 'John Q. Brown,I',
         'Linda Rosa Smith-Johnson', 'Anistasio Somoza Debayle')
parsed <- parseName(tst)

tst2 <- matrix(c('Smith', 'Joe', 'Gomez', 'Teresa Angelica Sanchez de',
  'Brown', 'John, Jr.', 'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Debayle', 'Anistasio Somoza'),
  ncol=2, byrow=TRUE)
# NOTE:  This last example is in the Spanish tradition
# and is handled incorrectly by the current algorithm.
# The correct answer should be "Somoza Debayle", "Anistasio",
# but that would complicate the algorithm excessively for now.
colnames(tst2) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(parsed, tst2)
\dontshow{)}

##
## 2.  Parse "surname, given name" format
##
tst3 <- c('Smith, Joe', 'Sanchez de Gomez, Teresa Angelica',
     'Brown, John, Jr.', 'Brown, John W., III', 'Brown, John Q., I',
     'Smith-Johnson, Linda Rosa', 'Somoza Debayle, Anistasio')
tst4 <- parseName(tst3, TRUE)

tst5 <- matrix(c('Smith', 'Joe', 'Sanchez de Gomez', 'Teresa Angelica',
  'Brown', 'John, Jr.', 'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Somoza Debayle', 'Anistasio'),
  ncol=2, byrow=TRUE)
colnames(tst5) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(tst4, tst5)
\dontshow{)}

}
\keyword{manip}
