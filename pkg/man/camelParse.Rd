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
camelParse(x)
}
\arguments{
  \item{x}{
    a character vector
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
         'EducationNational DefenseOther Committee')
tst. <- camelParse(tst)

\dontshow{stopifnot(}
all.equal(tst., list(c('Smith, John', 'John Smith'),
    c('Education', 'National Defense', 'Other Committee')) )
\dontshow{)}
}
\keyword{manip}
