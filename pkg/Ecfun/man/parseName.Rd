\name{parseName}
\alias{parseName}
\title{
  Parse surname and given name
}
\description{
  Identify the presumed surname in a character string assumed to
  represent a name and return the result in a character matrix with
  "surname" followed by "givenName".  If only one name is provided 
  (without punctuation), it is assumed to be the givenName;  
  see Wikipedia, 
  \href{https://en.wikipedia.org/wiki/Given_name}{"Given name"} 
  and \href{https://en.wikipedia.org/wiki/Surname}{"Surname"}.  
}
\usage{
parseName(x, surnameFirst=(median(regexpr(',', x))>0),
          suffix=c('Jr.', 'I', 'II', 'III', 'IV', 'Sr.', 'Dr.', 
                   'Jr', 'Sr'),
          fixNonStandard=subNonStandardNames, 
          removeSecondLine=TRUE, 
          namesNotFound="attr.replacement", ...)
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
  \item{removeSecondLine}{
    logical:  If TRUE, delete anything following "\\n" and return it as
    an attribute "secondLine".  
  }
  \item{namesNotFound}{
    character vector passed to \code{subNonStandardNames} and used 
    to compute any "namesNotFound" attribute of the object returned 
    by \code{parseName}.  
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
  a character matrix with two columns:  surname and givenName.  
  
  This matrix also has a "namesNotFound" attribute if one is 
  returned by \code{subNonStandardNames}.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}
  \code{\link{identity}}
  \code{\link{subNonStandardNames}}
}
%\references{}
\examples{
##
## 1.  Parse standard first-last name format
##
tstParse <- c('Joe Smith (AL)', 'Teresa Angelica Sanchez de Gomez',
         'John Brown, Jr.', 'John Brown Jr.',
         'John W. Brown III', 'John Q. Brown,I',
         'Linda Rosa Smith-Johnson', 'Anastasio Somoza Debayle',
         'Ra_l Vel_zquez', 'Sting', 'Colette, ')
%library(Ecdat)
parsed <- parseName(tstParse)

tstParse2 <- matrix(c('Smith', 'Joe', 'Gomez', 'Teresa Angelica Sanchez de',
  'Brown', 'John, Jr.', 'Brown', 'John, Jr.',
  'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Debayle', 'Anastasio Somoza',
  'Velazquez', 'Raul', '', 'Sting', 'Colette', ''),
  ncol=2, byrow=TRUE)
# NOTE:  The 'Anastasio Somoza Debayle' is in the Spanish tradition
# and is handled incorrectly by the current algorithm.
# The correct answer should be "Somoza Debayle", "Anastasio".
# However, fixing that would complicate the algorithm excessively for now.
colnames(tstParse2) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(parsed, tstParse2)
\dontshow{)}

##
## 2.  Parse "surname, given name" format
##
tst3 <- c('Smith (AL),Joe', 'Sanchez de Gomez, Teresa Angelica',
     'Brown, John, Jr.', 'Brown, John W., III', 'Brown, John Q., I',
     'Smith-Johnson, Linda Rosa', 'Somoza Debayle, Anastasio',
     'Vel_zquez, Ra_l', ', Sting', 'Colette,')
tst4 <- parseName(tst3)

tst5 <- matrix(c('Smith', 'Joe', 'Sanchez de Gomez', 'Teresa Angelica',
  'Brown', 'John, Jr.', 'Brown', 'John W., III', 'Brown', 'John Q., I',
  'Smith-Johnson', 'Linda Rosa', 'Somoza Debayle', 'Anastasio',
  'Velazquez', 'Raul', '','Sting', 'Colette',''),
  ncol=2, byrow=TRUE)
colnames(tst5) <- c("surname", 'givenName')

\dontshow{stopifnot(}
all.equal(tst4, tst5)
\dontshow{)}

##
## 3.  secondLine 
##
L2 <- parseName(c('Adam\\n2nd line', 'Ed  \\n --Vacancy', 'Frank'))

# check 
L2. <- matrix(c('', 'Adam', '', 'Ed', '', 'Frank'), 
              ncol=2, byrow=TRUE)
colnames(L2.) <- c('surname', 'givenName')
attr(L2., 'secondLine') <- c('2nd line', ' --Vacancy', NA)
\dontshow{stopifnot(}
all.equal(L2, L2.)
\dontshow{)}

##
## 4.  Force surnameFirst when in a minority 
##
snf <- c('Sting', 'Madonna', 'Smith, Al')
SNF <- parseName(snf, surnameFirst=TRUE)

# check 
SNF2 <- matrix(c('', 'Sting', '', 'Madonna', 'Smith', 'Al'), 
               ncol=2, byrow=TRUE)
colnames(SNF2) <- c('surname', 'givenName')               
\dontshow{stopifnot(}
all.equal(SNF, SNF2)
\dontshow{)}

##
## 5.  nameNotFound
##
noSub <- parseName('xx_x')

# check 
noSub. <- matrix(c('', 'xx_x'), 1)
colnames(noSub.) <- c('surname', 'givenName')               
attr(noSub., 'namesNotFound') <- 'xx_x'
\dontshow{stopifnot(}
all.equal(noSub, noSub.)
\dontshow{)}
}
\keyword{manip}
