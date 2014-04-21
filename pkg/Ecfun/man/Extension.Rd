\name{Extension}
\alias{Extension}
\title{
  Extract the extension from filenames
}
\description{
  Return the last character string from \code{strpslit(filenames, split,
    ...)}. Return \code{NA} if \code{split} is not found.
}
\usage{
Extension(filename, split='\\\\.', noExtensionError=TRUE, ...)
}
\arguments{
  \item{filename}{
    character vector
  }
  \item{split}{
    \code{split} argument for \code{\link{strsplit}}.
  }
  \item{noExtensionError}{
    If TRUE, throw an error if a \code{filename} does not have an
    extension (i.e., does not include \code{split}) or if the extension
    is ''.  If 'warn', issue a warning.  If FALSE, ignore.
  }
  \item{\dots}{
    optional arguments for \code{\link{grep}} or \code{\link{regexpr}}.
    (Not tested.)
  }
}
\details{
  1.  fn <- strsplit(filename, split, ...)

  2.  Ext <- sapply(fn, tail, n=1)

  3.  Fix cases where split is not present or is at the end.

  NOTE:  If \code{split} ends in '\$', that matches the end of the
  string.  In that case, \code{Extension} returns the segment before the
  end, if any.  (Not tested.)
}
%\source{ }
\value{
  a character vector of the extensions found.  Return \code{NA} if
  \code{split} is not found.
}
\references{
  \code{\link[raster]{extension}} returns '.txt' from 'filename.txt' and
  also has an assignment function.  The \code{Extension} function
  documented herein returns 'txt' not '.txt'.
}
\author{Spencer Graves}
\seealso{
  \code{\link[raster]{extension}}
  \code{\link{strsplit}}
}
\examples{
Filenames <- c('a', 'a.', 'a.txt')

Extns <- Extension(Filenames, noExtensionError='warn')

\dontshow{stopifnot(}
all.equal(Extns, c(NA, '', 'txt'))
\dontshow{)}
}
\keyword{manim}