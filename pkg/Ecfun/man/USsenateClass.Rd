\name{USsenateClass}
\alias{USsenateClass}
\title{
  Election Class given state and surname of a US Senator
}
\description{
  For all indiviuals in \code{x} with \code{houseSenate} == 
  "Senate", look up their \code{state} and \code{surname} 
  in the reference table \code{senate} and return their 
  \code{Class}.  For indivuals not found in \code{senate}, 
  return \code{x[[district]]}.

  Senate classes 1, 2 and 3 have their normal elections in 
  6-year cycles including 2000, 2002, and 2004 (or 2018, 
  2020, and 2022), respectively.  When vacancies occur out 
  of cycle, the vacancy is first filled with appointment 
  by the governor of the state, and an election to fill 
  that seat occurs in the next even-numbered year.  In 
  particular, the class of that seat does not change.

  For example, 
  \href{http://en.wikipedia.org/wiki/Jim_DeMint}{South
    Carolina Senator Jim DeMint} resigned effective 
  January 1, 2013. 
  \href{http://en.wikipedia.org/wiki/Nikki_Haley}{South 
    Carolina Governor Nikki Haley} appointed
  \href{http://en.wikipedia.org/wiki/Tim_Scott}{Tim Scott} 
  to serve until a special election in 2014.  This is a 
  Class 3 seat, which means that another election for that 
  seat would occur in 2016.
}
\usage{
USsenateClass(x, senate=readUSsenate(),
   Office='Office', state='state',
   surname='surname', district='district', 
   senatePattern='^Senate')
}
\arguments{
  \item{x}{
    \code{\link{data.frame}} with character or factor 
          columns
    \code{Office}, \code{state}, \code{surname}, and
    \code{district}.
  }
  \item{senate}{
    \code{\link{data.frame}} as returned by 
        \code{\link{readUSsenate}}.
  }
  \item{Office}{
    name of a character or factor variable \code{x} in 
    which the members of the US Senate can be identifed by 
    \code{grep(senatePattern, x[, Office])}.
  }
  \item{state}{
    Standard 2-letter abbreviation for the state of the US
  }
  \item{surname}{
    the name of a column of \code{x} containing the surname
  }
  \item{district}{
    name of a column of \code{x} containing the number of 
    the district in the US House.  For states with only one 
    representataive, this may be 0.
  }
  \item{senatePattern}{
    a regular expression for identifying the senators from 
    \code{x[, Office]}.
  }
}
\details{
  The current algorithm may fail if both senators in a 
  \code{state} have the same \code{surname}.
}
%\source{ }
\value{
  a \code{\link{data.frame}} with one row for each row 
  of \code{x} and the following columns:

  \item{incumbent}{
    logical vector:  \code{NA} if Office == 'house'.  If 
    Office == 'senate', then \code{TRUE} if 
    \code{state:surname} found in \code{senate} and
    \code{FALSE} otherwise.
  }
  \item{District}{
    a character vector containing the desired \code{Class} 
    for all US Senators found in \code{senate} or a guess 
    at the \code{Class} for non-incumbents.  For members 
    of the \code{House}, this returned the previous content 
    of \code{x[[District]]}.

    NOTES:

    1.  Incumbents can be missed if the spelling of the 
    surname is different between \code{x} and \code{senate}.  
    This can occur with, for example, Spanish surnames 
    containing an accent.

    2.  If one but not two incumbents is found, others are 
    currently assigned to the class of an incumbent not found.  
    This could be a mistake, because the person could be a 
    previous incumbent or could have lost to the incumbent in 
    the last election.
  }
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{readUSsenate}}
}
\examples{
tst <- data.frame(Office=factor(
            c("House", "Senate", "Senate", 'Senate')),
        state=factor(c('SC', 'SC', 'SC', 'NY')),
        surname=c("Jones", "DeMint", "Graham", 'Smith'),
        district=c("9", NA, NA, NA),
        stringsAsFactors=FALSE)
if(!fda::CRAN()){                  
tst. <- USsenateClass(tst)

chk <- data.frame(incumbent=c(NA, FALSE, TRUE, FALSE),
                  district=c("9", "3", "2", "1 or 3"),
                  stringsAsFactors=FALSE)
\dontshow{stopifnot(}
all.equal(tst., chk)
\dontshow{)}

##
## test with names different from the default
##
tst2 <- tst
names(tst2) <- letters[1:4]
tst2. <- USsenateClass(tst2, Office='a',
           state='b', surname='c', district='d')
\dontshow{stopifnot(}
all.equal(tst., tst2.)
\dontshow{)}
}
}
\keyword{manip}