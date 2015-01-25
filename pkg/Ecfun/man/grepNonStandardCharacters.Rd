\name{grepNonStandardCharacters}
\alias{grepNonStandardCharacters}
\title{
  grep for nonstandard characters
}
\description{
  Return the indices of elements of \code{x} containing characters
  that are not in \code{standardCharacters}.
}
\usage{
grepNonStandardCharacters(x, value=FALSE,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   ... )
}
\arguments{
  \item{x}{
    character vector in which it is desired to identfy elements
    containing characters not in \code{standardCharacters}.
  }
  \item{value}{
    logical:  \code{TRUE} to return the values found in \code{x},
    \code{FALSE} to return their indices.
  }
  \item{standardCharacters}{
    Characters to overlook in \code{x} to identify anything not in
    \code{standardCharacters}.
  }
  \item{\dots}{ optional arguments for \code{\link{regexpr}} }
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
  \code{\link[stringi]{stringi-package}}
  \code{\link{grep}},
  \code{\link{regexpr}},
  \code{\link{subNonStandardCharacters}},
  \code{\link[tools]{showNonASCII}}
}
%\references{}
\examples{
Names <- c('Raul', 'Ra`l', 'Torres,Raul', 'Torres, Raul')
#  confusion in character sets can create
#  names like Names[2]

chk <- grepNonStandardCharacters(Names)

\dontshow{stopifnot(}
all.equal(chk, 2)
\dontshow{)}

chkv <- grepNonStandardCharacters(Names, TRUE)

\dontshow{stopifnot(}
all.equal(chkv, 'Ra`l')
\dontshow{)}

}
\keyword{manip}
