\name{asNumericDF}
\alias{asNumericChar}
\alias{asNumericDF}
\title{
  Coerce to numeric dropping commas and info after a blank
}
\description{
  Delete commas (thousand separators) and drop information after a
  blank, then coerce to numeric and order the rows by the
  \code{orderBy}.  Some Excel imports include commas as thousand
  separators;  this replaces any commas with char(0), ''.  Also, some
  character data includes footnote references following the year.  Table
  F-1 from the US Census Bureau needs all three of these features:  It
  needs \code{orderBy}, because the most recent year appears first, just
  the opposite of most other data sets where the most recent year
  appears last.  It has footnote references following a character string
  indicating the year.  And it includes commas as thousand separators.
}
\usage{
asNumericChar(x)
asNumericDF(x, keep=function(x)any(!is.na(x)), orderBy)
}
\arguments{
  \item{x}{
    For \code{asNumericChar}, this is a character vector to be converted
    to numeric after \code{gsub(',', '', x)}.  For \code{asNumericDF},
    this is a \code{data.frame} with all character columns to be
    converted to numerics.
  }
  \item{keep}{
    something to indicate which columns to keep
  }
  \item{orderBy}{
    Which columns to order the rows of \code{x[, keep]} by
  }
}
\details{
  1.  Replace commas by nothing

  2.  strsplit on ' ' and take only the first part, thereby eliminating
  the footnote references.

  3.  Replace any blanks with NAs

  4.  as.numeric

  5.  lapply(x, 1-4)

  6. order the rows;  by default, ascending on the first column
}
\value{
  all numeric data.frame
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{scan}}
  \code{\link{gsub}}
  \code{\link{Quotes}}
}
%\references{}
\examples{

fakeF1 <- data.frame(yr=c('1948', '1947 (1)'),
                     q1=c('1,234', ''), duh=rep(NA, 2) )
nF1 <- asNumericDF(fakeF1)

nF1. <- data.frame(yr=asNumericChar(fakeF1$yr),
                   q1=asNumericChar(fakeF1$q1))[2:1,]

# correct answer
row.names(nF1.) <- 2:1

nF1c <- data.frame(yr=1947:1948, q1=c(NA, 1234))
row.names(nF1c) <- 2:1

\dontshow{stopifnot(}
all.equal(nF1, nF1.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(nF1, nF1c)
\dontshow{)}
}
\keyword{manip}
