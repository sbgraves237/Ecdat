\name{interpPairs}
\alias{interpPairs}
\title{
  interpolate between pairs of vectors in a list
}
\description{
  Find elements such as \code{x.0} and \code{x.1} of \code{object}
  indentified by regular expressions in \code{pairs[1:2]}.  Return
  \code{object} with each pair replaced by a single element with a name
  derived applying \code{\link{sub}} to convert, e.g., \code{x.0} to
  \code{x}, where \code{x} is obtained by linear interpolation using
  \code{proportion} between \code{x.0} and \code{x.1}.  Rows with
  \code{proportion} outside \code{validProportion} are dropped in
  \code{x} and any other elements of \code{object} with the same length
  as \code{x}.
}
\usage{
interpPairs(object, proportion,
            pairs=c('1'='\\\\.0$', '2'='\\\\.1$', replacement=''),
            validProportion=0:1 )
}
\arguments{
  \item{object}{
    A \code{list} or \code{data.frame} with column names possibly
    matching \code{suffixes}.  When one is found without the other, the
    one is returned with a warning, without its suffix.  Thus, if
    \code{x.0} is found without \code{x.1}, \code{x.0} is returned as an
    element of the output \code{list}.
  }
  \item{proportion}{
    a numeric vector assumed to lie between 0 and 1 specifying how far
    to go from suffixes[1] to suffixes[2].  For example, if \code{x.0}
    and \code{x.1} are found, \code{x} = \code{x.0 + proportion * (x.1 -
      x.0)}.  Rows of \code{x} and any other element of \code{object} of
    the same length are dropped for any \code{proportion} outside
    \code{validProportion}.
  }
  \item{pairs}{
    a character vector of two regular expressions to identify elements
    of \code{object} between which to interpolate and a
    \code{replacement} for use in \code{\link{sub}} to convert each
    \code{pairs[1:2]} name found to the desired name of the
    interpolation.
  }
  \item{validProportion}{
    Range of values of \code{proportion} to retain, as noted with the
    discussion of the \code{object} argument.
  }
}
\details{
  1.  Find names matching \code{pairs[1:2]}.

  2.  Convert each name found to the target name using
  \code{sub(pairs[i], pairs[3], ...)} for i = 1:2.

  3.  Create \code{Dat} = a \code{data.frame} of all matches found along
  with other columns of \code{object} of the same length.

  4.  Delete all rows of \code{Dat} with \code{proportion} outside
  \code{validProportion}.

  5.  Replace all pairs in \code{Dat} with the desired linear
  interpolation.  If any names are found without a match (e.g.,
  \code{x.0} without \code{x.1}), change the name to the target and
  issue a warning.

  4.  Return the modified \code{Dat} after merging with the remainder of
  \code{object}.
}
\value{
  a \code{list} with elements containing the interpolation results.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{animate}}
}
%\references{}
\examples{
tstList <- list(x.0=1:4, y.0=5:8, y.1=9:6, x.1=9,
                ignore=letters, col=1:4)
xy <- interpPairs(tstList, 0.1)

xy0 <- interpPairs(tstList[-4], c(-1, 0, 1, 2) )
xy02 <- interpPairs(tstList[c(2, 4)], 0.1)
xyz <- interpPairs(list(x=1:4), c(-1, 0, 1, 2))
x0 <- interpPairs(list(), 0:1)
# with elements of class call
xc <- interpPairs(list(x=1:3, y=quote(x+sin(pi*x/6))), 0:1)

# Answers
xy. <- list(x=1:4 + 0.1*(9-1:4),
            y=5:8 + 0.1*(9:6-5:8),
            col=1:4, ignore=letters)
xy0. <- list(x=2:3, y=6:7, col=2:3, ignore=letters)
xy02. <- list(y=5:8, x=rep(9, 4))
xyz. <- list(x=2:3)
x0. <- list()
names(x0.) <- character(0)

xc. <- list(x=1:3, y=1:3+sqrt(c(1, 3, 4)/4))

\dontshow{stopifnot(}
all.equal(xy, xy.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(xy0, xy0.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(xy02, xy02.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(xyz, xyz.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(x0, x0.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(xc, xc.)
\dontshow{)}
}
\keyword{manip}
