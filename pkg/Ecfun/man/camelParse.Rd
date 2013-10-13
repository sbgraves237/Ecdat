\name{camelParse}
\alias{camelParse}
\title{
  Split a character string where a capital letter follows a lowercase
  letter
}
\description{
  Split a character string where a capital letter follows a lowercase
  letter.
}
\usage{
camelParse(x, except=c('De', 'Mc', 'Mac'))
}
\arguments{
  \item{x}{
    a character vector
  }
  \item{except}{
    character vector giving exceptions:  If any of these are found,
    ignore and look for the next one
  }
}
\details{
  Find all places where a lowercase letter is followed by a capital.

  Split on those points
}
\value{
  list of character vectors
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}
}
%\references{}
\examples{
tst <- c('Smith, JohnJohn Smith',
         'EducationNational DefenseOther Committee',
         'McCain, JohnJohn McCain')
tst. <- camelParse(tst)

\dontshow{stopifnot(}
all.equal(tst., list(c('Smith, John', 'John Smith'),
    c('Education', 'National Defense', 'Other Committee'),
    c('McCain, John', 'John McCain') ) )
\dontshow{)}
}
\keyword{manip}
