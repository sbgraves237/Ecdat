\name{asNumericDF}
\alias{asNumericChar}
\alias{asNumericDF}
\title{
  Coerce to numeric dropping commas and info after a blank
}
\description{
  Delete a leading dollar sign plus commas (thousand separators)
  and drop information after a blank (other than leadng blanks), 
  then coerce to numeric.  
  
  For a \code{\link{data.frame}}, apply to all columns, drop 
  non-numeric columns, and order the rows by the \code{orderBy}.  
  Some Excel imports include commas as thousand separators;  
  this replaces any commas with char(0), ''.  
  
  Similarly, if "\%" is found as the last character in any field,
  drop the percent sign and divide the resulting numeric conversion 
  by 100 to convert to proportion.  
  
  Also, some character data includes footnote references following 
  the year.  
  
  Table F-1 from the US Census Bureau needs all three of these 
  features:  It needs \code{orderBy}, because the most recent year 
  appears first, just the opposite of most other data sets where 
  the most recent year appears last.  It has footnote references 
  following a character string indicating the year.  And it 
  includes commas as thousand separators.
}
\usage{
asNumericChar(x)
asNumericDF(x, keep=function(x)any(!is.na(x)), orderBy=NA)
}
\arguments{
  \item{x}{
    For \code{asNumericChar}, this is a character vector to be
    converted to numeric after \code{gsub(',', '', x)}.  For
    \code{asNumericDF}, this is a \code{data.frame} with all
    character columns to be converted to numerics.
  }
  \item{keep}{
    something to indicate which columns to keep
  }
  \item{orderBy}{
    Which columns to order the rows of \code{x[, keep]} by.  
    Default is to keep the input order.  
  }
}
\details{
  1.  Replace commas by nothing

  2.  strsplit on ' ' and take only the first part, thereby
  eliminating the footnote references.

  3.  Replace any blanks with NAs

  4.  as.numeric

  5.  For asNumericDF:  lapply(x, 1-4), then order the rows
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
  \code{\link[tis]{stripBlanks}}
}
%\references{}
\examples{
##
## 1.  simple example 
##
fakeF1 <- data.frame(yr=c('1948', '1947 (1)'),
                     q1=c(' 1,234 ', ''), duh=rep(NA, 2), 
                     dol=c('$1,234', ''), 
                     pct=c('1\%', '2\%'))
nF1 <- asNumericDF(fakeF1)

nF1. <- data.frame(yr=asNumericChar(fakeF1$yr),
                   q1=asNumericChar(fakeF1$q1), 
                   dol=asNumericChar(fakeF1$dol), 
                   pct=c(.01, .02))

nF1c <- data.frame(yr=1948:1947, q1=c(1234, NA), 
                   dol=c(1234, NA), pct=c(.01, .02))

\dontshow{stopifnot(}
all.equal(nF1, nF1.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(nF1., nF1c)
\dontshow{)}

##
## 2.  orderBy=1:2
##
nF. <- asNumericDF(fakeF1, orderBy=1:2)

\dontshow{stopifnot(}
all.equal(nF., nF1c[2:1,])
\dontshow{)}
}
\keyword{manip}
