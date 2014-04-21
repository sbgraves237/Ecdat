\name{whichAeqB}
\alias{whichAeqB}
\title{
  Index of a single match
}
\description{
  Return which(A \%in\% B) if it has length 1;  give an error message
  otherwise.
}
\usage{
whichAeqB(A, B, errNoMatch='no match',
                      err2Match='more than one match')
}
\arguments{
  \item{A}{
    A vector which may have a single match in \code{B}.
  }
  \item{B}{
    A vector of possible matches for \code{A}.
  }
  \item{errNoMatch}{
    a character string:  error message if no match found.
  }
  \item{err2Match}{
    a character string:  error message if multiple matches found.
  }
}
%\details{}
\value{
  a single integer giving the index of the match in \code{A}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{interpPairs}}
}
%\references{}
\examples{
a2b <- whichAeqB(letters, 'b')

\dontshow{stopifnot(}
all.equal(a2b, 2)
\dontshow{)}
}
\keyword{plot}
