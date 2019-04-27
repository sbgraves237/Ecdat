\name{asNumericDF}
\alias{asNumericChar}
\alias{asNumericDF}
\title{
  Coerce to numeric dropping commas and info after a blank
}
\description{
  For \code{asNumericChar}, delete leading
  blanks and a leading dollar sign plus
  commas (thousand separators) and drop
  information after a blank (other than
  leadng blanks), then coerce to numeric or
  to factors, Dates, or POSIXct as desired.
  
  For a \code{\link{data.frame}}, apply 
  \code{asNumericChar} to all columns and
  drop columns except those in \code{keep},
  \code{ignore}, \code{factors}, 
  \code{Dates}, \code{POSIX} and 
  \code{MSdates}.

  Then order the rows by the 
  \code{orderBy} column.  Some Excel 
  imports include commas as thousand
  separators;  this replaces any commas
  with char(0), '', before trying to
  convert to numeric.  
  
  Similarly, if "\%" is found as the last 
  character in any field, drop the percent
  sign and divide the resulting numeric
  conversion by 100 to convert to 
  proportion.  
  
  Also, some character data includes 
  footnote references following the year.  
  
  For example Table F-1 from the US Census 
  Bureau needs all three of these numeric
  conversion features:  It needs 
  \code{orderBy}, because the most recent 
  year appears first, just the opposite of
  most other data sets where the most
  recent year appears last.  It has
  footnote references following a character
  string indicating the year.  And it
  includes commas as thousand separators.
}
\usage{
asNumericChar(x, leadingChar='^\\\\$', 
    suppressChar=',', pctChar='\%$', 
    class.=NULL, format.=NULL)
asNumericDF(x, keep=
    function(x)any(!is.na(x)), 
  orderBy=NA, ignore=NULL, factors=NULL, 
  Dates=NULL, POSIX=NULL, MSdates=NULL, 
  format.=NULL, leadingChar='^\\\\$', 
  suppressChar=',', pctChar='\%$')
}
\arguments{
  \item{x}{
    For \code{asNumericChar}, this is a
    character vector to be converted to
    numeric after \code{gsub(',', '', x)}.
    
    For \code{asNumericDF}, this is a
    \code{data.frame} with all character 
    columns to be converted to numerics.
  }
  \item{keep}{
    something to indicate which columns to 
    keep, in addition to columns specified
    in \code{ignore}, \code{factors},
    \code{Dates}, and \code{POSIX}.  
  }
  \item{orderBy}{
    Which columns to order the rows of
    \code{x[, keep]} by.  Default is to
    keep the input order.  
  }
  \item{ignore}{
    vector identifying columns of \code{x} 
    to ignore, i.e., to keep and not
    attempt to convert to another data 
    type.  
  }
  \item{factors}{
    vector indicating columns of \code{x} 
    to convert to \code{\link{factor}}
  } 
  \item{Dates}{
    vector indicating columns of \code{x} 
    to convert using 
    \code{\link{as.Date}}(, \code{format}).
  }
  \item{POSIX}{
    vector indicating columns of \code{x} 
    to convert using 
    \code{\link{as.POSIXct}}(, \code{format}).  
  }
  \item{class.}{
    Desired class of output.  Default is 
    \code{\link{numeric}}.  
  }
  \item{format.}{
    Character vector of length 1 to pass 
    as argument \code{format} to 
    \code{\link{as.Date}} and / or 
    \code{\link{as.POSIXct}} for conversion
    from \code{\link{character}}.  
    
    For \code{Dates}, \code{\link{as.Date}}
    is first tried with format = 
    '\%Y-\%m-\%d', then with '\%Y/\%m/\%d',
    '\%m-\%d-\%Y', and '\%m/\%d/\%Y'.  The
    conversion with the fewest NAs is kept.
    If two match for numbers of NAs, the
    one with the minimum absolute 
    deviations from as.Date1970(0) is used.
  }
  \item{MSdates}{
    The names or numbers identifying
    columns of x identifying dates as
    integer numbers of days since 
    1899-12-31.  \href{https://support.office.com/en-us/article/Add-sum-or-subtract-dates-9818b3f7-aa67-4008-8dff-3c3b503886af?ui=en-US&rs=en-US&ad=US}{In 
    Microsoft Excel, dates are stored in
    that format}.  Tests on 2016-08-10
    revealed that 
    \code{\link[gdata]{read.xls}} did not 
    read those dates properly (using gdata
    version 2.17.0), while 
    \code{\link[openxlsx]{read.xlsx}}
    read them as integers using openxlsx 
    3.0.0. This capability will convert
    those integers to Dates using 
    \code{as.Date(., origin = 
    as.Date('1899-12-31'))}.  
  }
  \item{leadingChar}{
    A regular expression passed to 
    \code{\link{grep}} and 
    \code{\link{sub}} to replace something
    like an initial dollar sign with
    \code{character(0)}.  
  }
  \item{suppressChar}{
    a regular expression passed to 
    \code{\link{gsub}} to replace all 
    occurrances of something like "," (a 
    thousands separator in the U.S.) with 
    \code{character(0)}.  
  }
  \item{pctChar}{
    A regular expression passed to 
    \code{\link{grep}} to identify percent 
    columns.  \code{pctChar} is then 
    passed to \code{\link{sub}} to replace 
    \code{pctChar} with 
    \code{character(0)}, and the converted
    numbers are then divided by 100 to
    convert them to proportions.  
  } 
}
\details{
For asNumericChar:  

  1.  Replace commas by nothing

  2.  strsplit on ' ' and take only the
  first part, thereby eliminating the
  footnote references.

  3.  Replace any blanks with NAs

  4.  as.numeric

for asNumericDF:  

  1.  Copy \code{x} to \code{X}.  
 
  2.  Confirm that \code{ignore}, 
  \code{factors}, \code{Dates}, and
  \code{POSIX} all refer to columns 
  of \code{x} and do not overlap.  
  [*** NOTE:  as of 2016-07-21, these
  checks have only been implemented
  for \code{ignore}.]
  
  3.  Convert \code{factors}, \code{Dates},
  and \code{POSIX}.  
  
  4.  Apply \code{asNumericChar} to all
  columns not in \code{ignore}, 
  \code{factors}, \code{Dates}, or
  \code{POSIX}.  
  
  5.  Keep columns specified by 
  \code{keep}.  
  
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
  \code{\link{as.numeric}}, 
  \code{\link{factor}}, 
  \code{\link{as.Date}}, 
  \code{\link{as.POSIXct}}
  \code{\link[openxlsx]{read.xlsx}}
}
\references{
  \href{https://support.office.com/en-us/article/Add-sum-or-subtract-dates-9818b3f7-aa67-4008-8dff-3c3b503886af?ui=en-US&rs=en-US&ad=US}{"Add (sum) or subtract dates;  Applies To: Excel 2013"}.  Microsoft.  (accessed 2016-08-11)
}
\examples{
##
## 1.  an example 
##
xDate <- as.Date('1970-01-01')+c(0, 365)
xPOSIX <- as.POSIXct(xDate)+c(1, 99)
xMSdate <- as.Date(1, 
    as.Date('1899-12-31'))+1:2
fakeF1 <- data.frame(yr=c('1948', 
            '1947 (1)'),
      q1=c(' 1,234 ', ''), duh=rep(NA, 2), 
      dol=c('$1,234', ''), 
      pct=c('1\%', '2\%'), 
      xDate=as.character(xDate, 
        format='\%m-\%d-\%Y'), 
      xPOSIX=as.character(xPOSIX, 
        format='\%m-\%d-\%Y \%H:\%M:\%S'), 
      xMSdate=2:3, junk=c('this is',
          'junk'))
            
# This converts the last 3 columns to NAs and drops them:   

str(nF1.1 <- asNumericChar(fakeF1$yr))
str(nF1.2 <- asNumericChar(fakeF1$q1))
str(nF1.3 <- asNumericChar(fakeF1$duh))

nF1 <- asNumericDF(fakeF1)
nF2 <- asNumericDF(fakeF1, Dates=6, 
    MSdate='xMSdate', 
    ignore=c('junk', 'xPOSIX'), 
    format.='\%m-\%d-\%Y')
nF3 <- asNumericDF(nF2, POSIX='xPOSIX', 
    ignore=c(5,7:8), 
    format.='\%m-\%d-\%Y \%H:\%M:\%S')
            
# check 
nF1. <- data.frame(yr=
        asNumericChar(fakeF1$yr),
      q1=asNumericChar(fakeF1$q1), 
      dol=asNumericChar(fakeF1$dol), 
      pct=c(.01, .02), xMSdate=2:3)

nF1c <- data.frame(yr=1948:1947, 
      q1=c(1234, NA), dol=c(1234, NA), 
      pct=c(.01, .02), xMSdate=2:3)

\dontshow{stopifnot(}
all.equal(nF1, nF1.)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(nF1., nF1c)
\dontshow{)}

nF3c <- data.frame(yr=1948:1947, 
      q1=c(1234, NA), dol=c(1234, NA), 
      pct=c(.01, .02), xDate=xDate, 
      xPOSIX=xPOSIX, xMSdate=xMSdate, 
      junk=fakeF1$junk)
\dontshow{stopifnot(}
all.equal(nF3, nF3c)
\dontshow{)}

## 
## 2.  as.Date default example
##
  xD <- asNumericChar(
    as.character(xDate), class.='Date')
\dontshow{stopifnot(}
  all.equal(xDate, xD)
\dontshow{)}

##
## 4.  as.POSIXct default example 
##
xP <- asNumericChar(as.character(xPOSIX),
                    class.='POSIXct')
\dontshow{stopifnot(}
all.equal(xPOSIX, xP)
\dontshow{)}

##
## 5.  orderBy=1:2
##
nF. <- asNumericDF(fakeF1, orderBy=1:2)

\dontshow{stopifnot(}
all.equal(nF., nF1c[2:1,])
\dontshow{)}
}
\keyword{manip}
