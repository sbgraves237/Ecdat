\name{CensusF1decrypt}
\alias{CensusF1decrypt}
\title{
  Decrypt US Census Table F-1. Income Limits for Each Fifth and Top 5
  Percent of Families
}
\description{
  Delete commas (thousand separators) and footnote references from Table
  F-1 from the US Census Bureau giving the quintiles and the 95th
  percentile of US family income since 1947.
}
\usage{
CensusF1decrypt(x, activeCols=7)
}
\arguments{
  \item{x}{
    data.frame with columns (all character variables) Year,
    Number.thousands, quintile1, quintile2, quintile3, quintile4, p95.
  }
  \item{activeCols}{
    maximum number of columns of x to process and retain.  This is
    useful in case the input process retains blank column(s).
  }
}
\details{
  1.  Replace commas by nothing

  2.  strsplit on ' ' and take only the first part, thereby eliminating
  the footnote references.

  3.  Replace any blanks with NAs

  4.  as.numeric

  5.  lapply 1-4 to the "rev" of the individual columns;  "rev" is
  needed, because the F1 table is kept with the most recent data
  in the first row, and we want the most recent in the last row.
}
\value{
  data.frame with activeCols numeric columns
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
nF1 <- CensusF1decrypt(fakeF1, activeCols=2)

# correct answer
nF1. <- data.frame(yr=1947:1948, q1=c(NA, 1234))

\dontshow{stopifnot(}
all.equal(nF1, nF1.)
\dontshow{)}
}
\keyword{manip}
