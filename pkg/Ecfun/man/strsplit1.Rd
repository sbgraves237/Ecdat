\name{strsplit1}
\alias{strsplit1}
\title{
  Split the first field 
}
\description{
  Split the first field from \code{x}, identified as all the 
  characters preceeding the first unquoted occurrence of 
  \code{split}.  
}
\usage{
strsplit1(x, split=',', Quote='"', ...)
}
\arguments{
  \item{x}{
    a character vector to be split 
  }
  \item{split}{
    the split character 
  }
  \item{Quote}{
    a quote character:  Occurrences of \code{split} between 
    pairs of \code{Quote} are ignored.  
  }
  \item{\dots}{
    optional arguments for grep  
  }
}
\details{
  This function was written to help parse data from the US 
  Department of Health and Human Services on 
  \href{https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf}{
  cyber-security breaches affecting 500 or more individuals}.  
  As of 2014-06-03 the csv version of these data included commas 
  in quotes that are not \code{sep} characters.  this function 
  was written to split the fields one at a time to allow manual
  processing to make it easier to correct parsing errors.  
  
  Algorithm:  

  1.  spl1 <- regexpr(split, x, ...)
  
  2.  Qt1 <- regexpr(Quote, x, ...)
  
  3.  For any (Qt1<spl1), look for 
  Qt2 <- regexpr(Quote, substring(x, Qt1+1)), then 
  look for 
  spl1 <- regexpr(split, substring(x, Qt1+Qt2+1))
  
  4.  out <- list(substr(x, 1, spl1-1), 
  substr(x, spl1+1))
}
\value{
  A list of length 2:  The first component of the list 
  contains the character strings found before the first unquoted
  occurrence of \code{split}.  The second component 
  contains the character strings remaining after the 
  characters up to the identified \code{split} are removed.   
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}
  \code{\link{substring}}
  \code{\link{grep}}
}
%\references{}
\examples{
chars2split <- c(qs00='abcdefg', qs01='abc,def', 
   qs10a='"abcdefg', qs10b='abc"defg', 
   qs1.1='"abc,def', qs20='"abc" def', 
   qs2.1='"ab,c" def', qs21='"abc", def', qs22.1='"a,b",c')    

split <- strsplit1(chars2split)

# answer
split. <- list(c(qs00='abcdefg', qs01='abc', qs10a='"abcdefg', 
   qs10b='abc"defg', qs1.1='"abc,def', qs20='"abc" def', 
   qs2.1='"ab,c" def', qs21='"abc"', qs22.1='"a,b"'), 
               c(qs00='', qs01='def', qs10a='', 
   qs10b='', qs1.1='', qs20='', qs2.1='', 
   qs21=' def', qs22.1='c') )
\dontshow{stopifnot(}
all.equal(split, split.)
\dontshow{)}
}
\keyword{manip}
