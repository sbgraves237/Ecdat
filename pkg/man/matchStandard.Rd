\name{matchStandard}
\alias{matchStandard}
\title{
  match ignorning nonstandard characters
}
\description{
  Return a logical vector indicating which elements of \code{x} are
  matched in \code{text} with anything not in \code{specialCharacters}
  matching anything.
}
\usage{
matchStandard(x, table, standardCharacters=
   c(letters, LETTERS, ' ','.', ', ', '\"', "\'", '-') )
}
\arguments{
  \item{x}{
    vector of values to be matched
  }
  \item{table}{
    vectors of values to be matched against
  }
  \item{standardCharacters}{
    Characters to consider in \code{x} and \code{table}, so characters
    not in \code{standardCharacters} are matched by anything.
  }
}
\details{

}
\value{
  a logical vector indicating the elements of \code{x} matched in
  \code{table}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{grep}}
}
%\references{}
\examples{
Names <- c('Raul', 'Ra`l', 'José', 'Jose', 'Ra', 'Joseph')
#  confusion in character sets can create]
#  names like Names[2]
chk1 <- matchStandard(Names[c(1, 3, 5)],
                      Names[c(2, 4, 6)])
\dontshow{stopifnot(}
#all.equal(chk1, c(TRUE, TRUE, FALSE))
\dontshow{)}
}
\keyword{manip}
