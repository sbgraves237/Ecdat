\name{asNumericDF}
\alias{asNumericDF}
\title{
  Coerce to numeric dropping commas and info after a blank
}
\description{
  Delete commas (thousand separators) and drop information after a blank
  (such as footnote references from Table F-1 from the US Census Bureau
  giving the quintiles and the 95th percentile of US family income since
  1947), then coerce to numeric and order the rows by the orderBy.
}
\usage{
asNumericDF(x, keep=function(x)any(!is.na(x)), orderBy)
}
\arguments{
  \item{x}{
    data.frame with all character columns to be converted to numerics
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

# correct answer
nF1. <- data.frame(yr=1947:1948, q1=c(NA, 1234))
row.names(nF1.) <- 2:1

\dontshow{stopifnot(}
all.equal(nF1, nF1.)
\dontshow{)}
}
\keyword{manip}
