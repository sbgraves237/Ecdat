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
chars2split <- c('abc,def', '"a,bc", def', 
    'none', ',end', 'start,')
split <- strsplit1(chars2split)
# answer

split. <- list(c('abc', '"a,bc"', 'none', '', 'start'), 
               c("def", " def",    '', 'end', ''))
\dontshow{stopifnot(}
all.equal(split, split.)
\dontshow{)}
}
\keyword{manip}
