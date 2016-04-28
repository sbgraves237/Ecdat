\name{asNumericDF}
\alias{asNumericChar}
\alias{asNumericDF}
\title{
  Coerce to numeric dropping commas and info after a blank
}
\description{
  For \code{asNumericChar}, delete leading blanks and a 
  leading dollar sign plus commas (thousand separators) 
  and drop information after a blank (other than leadng 
  blanks), then coerce to numeric or to factors, Dates, 
  or POSIXct as desired.  
  
  For a \code{\link{data.frame}}, apply to all columns
  and drop non-numeric columns except those in 
  \code{ignore}, \code{factors}, \code{Dates}, and 
  \code{POSIXct}.  Then order the rows by the 
  \code{orderBy} column.  Some Excel imports include 
  commas as thousand separators;  this replaces any 
  commas with char(0), '' before trying to convert 
  to numeric.  
  
  Similarly, if "\%" is found as the last character in 
  any field, drop the percent sign and divide the 
  resulting numeric conversion by 100 to convert to 
  proportion.  
  
  Also, some character data includes footnote 
  references following the year.  
  
  Table F-1 from the US Census Bureau needs all three 
  of these numeric conversion features:  It needs 
  \code{orderBy}, because the most recent year appears 
  first, just the opposite of most other data sets 
  where the most recent year appears last.  It has 
  footnote references following a character string 
  indicating the year.  And it includes commas as 
  thousand separators.
}
\usage{
asNumericChar(x)
asNumericDF(x, keep=function(x)any(!is.na(x)), 
    orderBy=NA, ignore=NULL, factors=NULL, 
    Dates=NULL, POSIX=NULL, format)
}
\arguments{
  \item{x}{
    For \code{asNumericChar}, this is a character vector to be
    converted to numeric after \code{gsub(',', '', x)}.  For
    \code{asNumericDF}, this is a \code{data.frame} with all
    character columns to be converted to numerics.
  }
  \item{keep}{
    something to indicate which columns to keep, in 
    addition to columns specified in \code{ignore}, 
    \code{factors}, \code{Dates}, and \code{POSIX}.  
  }
  \item{orderBy}{
    Which columns to order the rows of \code{x[, keep]} by.  
    Default is to keep the input order.  
  }
  \item{ignore}{
    vector identifying columns of \code{x} to 
    ignore, i.e., to keep and not attempt to 
    convert to another data type.  
  }
  \item{factors}{
    vector indicating columns of \code{x} to 
    convert to \code{\link{factor}}
  } 
  \item{Dates}{
    vector indicating columns of \code{x} to 
    convert using \code{\link{as.Date}}(, 
    \code{format}).  
  }
  \item{POSIX}{
    vector indicating columns of \code{x} to 
    convert using \code{\link{as.POSIXct}}(, 
    \code{format}).  
  }
  \item{format}{
    Character vector of length 1 to pass 
    as argument \code{format} to 
    \code{\link{as.Date}} and / or 
    \code{\link{as.POSIXct}} for conversion 
    from \code{\link{character}}.  
    
    For \code{Dates}, \code{\link{as.Date}} is 
    first tried with format = '\%Y-\%m-\%d', then 
    with '\%Y/\%m/\%d', '\%m-\%d-\%Y', and 
    '\%m/\%d/\%Y'.  
  }
}
\details{
For asNumericChar:  

  1.  Replace commas by nothing

  2.  strsplit on ' ' and take only the first part, thereby
  eliminating the footnote references.

  3.  Replace any blanks with NAs

  4.  as.numeric

for asNumericDF:  

  1.  Copy \code{x} to \code{X}.  
 
  2.  Confirm that \code{ignore}, \code{factors}, 
  \code{Dates}, and \code{POSIX} all refer to columns 
  of \code{x} and do not overlap.  
  
  3.  Convert \code{factors}, \code{Dates}, and 
  \code{POSIX}.  
  
  4.  Apply \code{asNumericChar} to all columns 
  not in \code{ignore}, \code{factors}, \code{Dates}, 
  or \code{POSIX}.  
  
  5.  Keep columns specified by \code{keep}.  
  
  6.  return the result.  
}
\value{
  a \code{\link{data.frame}}
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{scan}}
  \code{\link{gsub}}
  \code{\link{Quotes}}
  \code{\link[tis]{stripBlanks}}
  \code{\link{as.numeric}}, \code{\link{factor}}, 
  \code{\link{as.Date}}, \code{\link{as.POSIXct}}
}
%\references{}
\examples{
##
## 1.  an example 
##
xDate <- as.Date('1970-01-01')+c(0, 365)
xPOSIX <- as.POSIXct(xDate)+c(1, 99)
fakeF1 <- data.frame(yr=c('1948', '1947 (1)'),
            q1=c(' 1,234 ', ''), duh=rep(NA, 2), 
            dol=c('$1,234', ''), 
            pct=c('1\%', '2\%'), 
            xDate=as.character(xDate, format='\%m-\%d-\%Y'), 
            xPOSIX=as.character(xPOSIX), 
            junk=c('this is','junk'))
# This converts the last 3 columns to NAs and drops them:                       
nF1 <- asNumericDF(fakeF1)
nF2 <- asNumericDF(fakeF1, Dates='xDate', POSIX='xPOSIX', 
        ignore='junk')

# check 
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

nF2c <- data.frame(yr=1948:1947, q1=c(1234, NA), 
            dol=c(1234, NA), pct=c(.01, .02), 
            xDate=xDate, xPOSIX=xPOSIX, 
            junk=fakeF1$junk)
\dontshow{stopifnot(}
all.equal(nF2, nF2c)
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
