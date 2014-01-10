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
parseName(x, surnameFirst=(median(regexpr(',', x))>0),
          suffix=c('Jr.', 'I', 'II', 'III', 'IV', 'Sr.'),
          fixNonStandard=subNonStandardNames, ...)
}
\arguments{
  \item{x}{
    a character vector
  }
  \item{surnameFirst}{
    logical:  If TRUE, the surname comes first followed by a comma
    (","), then the given name.  If FALSE, parse the surname from a
    standard Western "John Smith, Jr." format.  If
    \code{missing(surnameFirst)}, use TRUE if half of the elements of
    \code{x} contain a comma.
  }
  \item{suffix}{
    character vector of strings that are NOT a surname but might appear
    at the end without a comma that would otherwise identify it as a
    suffix.
  }
  \item{fixNonStandard}{
    function to look for and repair nonstandard names such as names
    containing characters with accent marks that are sometimes mangled
    by different software.  Use \code{\link{identity}} if this is not
    desired.
  }
  \item{\dots}{ optional arguments passed to \code{fixNonStandard}}
}
\details{
  If \code{surnameFirst} is \code{FALSE}:

  1.  If the last character is ")" and the matching "(" is 3 characters
  earlier, drop all that stuff.  Thus, "John Smith (AL)" becomes "John
  Smith".

  2.  Look for commas to identify a suffix like Jr. or III;  remove and
  call the rest x2.

  3.  split <- strsplit(x2, " ")

  4.  Take the last as the surname.

  5.  If the "surname" found per 3 is in \code{suffix}, save to append
  it to the \code{givenName} and recurse to get the actual surname.

  NOTE:  This gives the wrong answer with double surnames written
  without a hyphen in the Spanish tradition, in which, e.g., "Anistasio
  Somoza Debayle", "Somoza Debayle" give the (first) surnames of
  Anistasio's father and mother, respectively:  The current algorithm
  would return "Debayle" as the surname, which is incorrect.

  6.  Recompose the rest with any suffix as the givenName.
}
\value{
  a character matrix with two columns:  surname and givenName
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}
  \code{\link{identity}}
}
%\references{}
\examples{
##
## 1.  Parse standard first-last name format
##
tst <- c('Joe Smith (AL)', 'Teresa Angelica Sanchez de Gomez',
         'John Brown, Jr.', 'John Brown Jr.',
         'John W. Brown III', 'John Q. Brown,I',
         'Linda Rosa Smith-Johnson', 'Anastasio Somoza Debayle',
         'Ra_l Vel_zquez')
parsed <- parseName(tst)

tst2 <- matrix(c('Smith', 'Joe', 'Gomez', 'Teresa Angelica Sanchez de',
  'Brown', 'John, Jr.', 'Brown', 'John, Jr.',
  'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Debayle', 'Anastasio Somoza',
  'Velazquez', 'Raul'),
  ncol=2, byrow=TRUE)
# NOTE:  This second to last example is in the Spanish tradition
# and is handled incorrectly by the current algorithm.
# The correct answer should be "Somoza Debayle", "Anastasio".
# However, fixing that would complicate the algorithm excessively for now.
colnames(tst2) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(parsed, tst2)
\dontshow{)}

##
## 2.  Parse "surname, given name" format
##
tst3 <- c('Smith (AL), Joe', 'Sanchez de Gomez, Teresa Angelica',
     'Brown,John, Jr.', 'Brown, John W., III', 'Brown, John Q., I',
     'Smith-Johnson, Linda Rosa', 'Somoza Debayle, Anastasio',
     'Vel_zquez, Ra_l')
tst4 <- parseName(tst3)

tst5 <- matrix(c('Smith', 'Joe', 'Sanchez de Gomez', 'Teresa Angelica',
  'Brown', 'John, Jr.', 'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Somoza Debayle', 'Anastasio',
  'Velazquez', 'Raul'),
  ncol=2, byrow=TRUE)
colnames(tst5) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(tst4, tst5)
\dontshow{)}
}
\keyword{manip}
