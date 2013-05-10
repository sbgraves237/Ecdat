\name{USsenateClass}
\alias{USsenateClass}
\title{
  Election Class given state and surname of a US Senator
}
\description{
  For all indiviuals in \code{x} with \code{houseSenate} == "Sen", look
  up their \code{state} and \code{surname} in the reference table
  \code{senate} and return their \code{Class}.  For indivuals not found
  in \code{senate}, return \code{x[["District"]]}.

  Senate classes 1, 2 and 3 have their normal elections in 6-year cycles
  including 2000, 2002, and 2004 (or 2012, 2008, and 2010),
  respectively.  When vacancies occur out of cycle, the vacancy is first
  filled with appointment by the governor of the state, and an election
  to fill that seat occurs in the next even-numbered year;  the class of
  that seat does not change.

  For example, \href{http://en.wikipedia.org/wiki/Jim_DeMint}{South
    Carolina Senator Jim DeMint} resigned effective January 1,
  2013. \href{http://en.wikipedia.org/wiki/Nikki_Haley}{South Carolina
    Governor Nikki Haley} appointed
  \href{http://en.wikipedia.org/wiki/Tim_Scott}{Tim Scott} to serve
  until a special election in 2014.  This is a Class 3 seat, which means
  that another election for that seat will occur in 2016.
}
\usage{
USsenateClass(x, senate=readUSsenate(),
   houseSenate='Office', state='District.state',
   surname='surname', District='District.number', senatePattern='^sen')
}
\arguments{
  \item{x}{
    \code{\link{data.frame}} with character or factor columns
    \code{houseSenate}, \code{state}, \code{surname}, and
    \code{District}.
  }
  \item{senate}{
    \code{\link{data.frame}} as returned by \code{\link{readUSsenate}}.
  }
  \item{houseSenate, state, surname, District}{
    the names of columns of \code{x}
  }
  \item{senatePattern}{
    a regular expression for identifying the senators from \code{x[,
      houseSenate]}.
  }
}
\details{
  The current algorithm may fail if both senators in a \code{state} have
  the same \code{surname}.
}
%\source{ }
\value{
  a character vector with length = \code{nrow(x)} containing the desired
  \code{Class} for all US Senators found in \code{senate} and the
  content of \code{x[["District"]]} for all others.
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{readUSsenate}}
}
\examples{
tst <- data.frame(Office=c("Rep", "Sen", "Sen", 'Sen'),
                  District.state=c('SC', 'SC', 'SC', 'NY'),
                  surname=c("Jones", "DeMint", "Graham", 'Smith'),
                  District.number=c("9", NA, NA, NA))
tst. <- USsenateClass(tst)

##
## test with names different from the default
##
tst2 <- tst
names(tst2) <- letters[1:4]
tst2. <- USsenateClass(tst2, houseSenate='a',
           state='b', surname='c', District='d')
\dontshow{stopifnot(}
all.equal(tst., tst2.)
\dontshow{)}

}
\keyword{manip}