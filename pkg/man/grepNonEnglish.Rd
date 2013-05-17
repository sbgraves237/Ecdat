\name{grepNonEnglish}
\alias{grepNonEnglish}
\title{
  grep for nonstandard characters
}
\description{
  Return the indices of elements of \code{x} containing characters that
  are not in \code{standardCharacters}.
}
\usage{
grepNonEnglish(x, standardCharacters=
   c(letters, LETTERS, ' ','.', ',', '\"', "\'", '-', '(', ')',
     '[', ']') )
}
\arguments{
  \item{x}{
    character vector in which it is desired to identfy elements
    containing characters not in \code{standardCharacters}.
  }
  \item{standardCharacters}{
    Characters to overlook in \code{x} to identify anything not in
    \code{standardCharacters}.
  }
}
\details{
  1.  x. <- strsplit(x, ''):  convert the input character vector to a
  list of vectors of character vectors with \code{nchar(x.[i])} == 1
    for i in 1:length(x).

  2.  sapply(x., ...) to identify all elements for which any element of
  x[[i]] is not in \code{standardCharacters}.
}
\value{
  an integer vector identifying all elements of \code{x} containing a
  character not in \code{standardCharacters}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{grep}}
}
%\references{}
\examples{
Names <- c('Raul', 'Ra`l', 'Torres,Raul', 'Torres, Raul')
#  confusion in character sets can create
#  names like Names[2]
chk <- grepNonEnglish(Names)

\dontshow{stopifnot(}
all.equal(chk, 2)
\dontshow{)}

}
\keyword{manip}
