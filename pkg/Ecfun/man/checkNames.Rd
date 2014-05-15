\name{checkNames}
\alias{checkNames}
\title{
 Check and return names  
}
\description{
  Check and return \code{\link{names}}.  If names are not 
  provided or are not unique, write a message and return 
  \code{\link{make.names}} consistent with \code{warn} and 
  \code{unique}.  
}
\usage{
checkNames(x, warn=0, unique=TRUE, 
   avoid=character(0), 
   message0=head(deparse(substitute(x), 25), 2), ...)
}
\arguments{
  \item{x}{
    an \R object suitable for \code{\link{names}}  
  }
  \item{warn}{
    Numeric code for how to treat problems, consistent 
    with the argument \code{warn} in \code{\link{options}}:  
    Negative to ignore, 0 to save and print later, 1 to 
    print as they occur, 2 or greater to convert to errors.  
  }
  \item{unique}{
    logical:  TRUE to check that \code{names(x)} are 
    unique.  Fix any duplicates with \code{\link{make.names}}.   
  }
  \item{avoid}{
    a vector of regular expressions to avoid adding in the 
    output of \code{\link{make.names}} with a companion 
    \code{replacement} when found.  
    
    Thus, \code{length(avoid)} must be a nonnegative even 
    integer, with \code{avoid[2*j-1]} providing the 
    \code{pattern} for \code{\link{regexpr}} and 
    \code{\link{sub}}, and \code{avoid[2*j]} providing 
    the \code{replacement}.  See the second example.      
  }
  \item{message0}{
    Base to prepend to any message 
  }
  \item{\dots}{ 
    optional arguments for \code{\link{make.names}}
  }
}
\details{
  1.  \code{namex <- names(x)}  
  
  2.  Check per \code{warn} and \code{unique}  
  
  3.  Return an appropriate version of \code{namex}
}
%\source{ }
\value{
  a character vector of the same length as \code{x}.  If any 
  problem is found, this character vector will have an 
  attribute \code{message} describing the problem found.  
  Message checking considers \code{unique} but ignores 
  \code{warn}.  
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{names}}
  \code{\link{make.names}}
  \code{\link{options}} for \code{warn}
}
\examples{
##
## 1.  standard operation with no names 
## 
tst1 <- checkNames(1:2)

# check 
tst1. <- make.names(character(2), unique=TRUE)
attr(tst1., 'message') <- paste(
  "1:2:  names = NULL; returning", 
  "make.names(character(length(x))), TRUE)")
\dontshow{stopifnot(}
all.equal(tst1, tst1.)
\dontshow{)}
##
## 2.  avoid=c('\\\\.0$', '\\\\.1$')
##
tst2 <-checkNames(1:2, 
    avoid=c('\\\\.0$', '.2', 
            '\\\\.1$', '.3') )

# check 
tst2. <-c('X', 'X.3')
attr(tst2., 'message') <- paste(
  "1:2:  names = NULL; returning", 
  "make.names(character(length(x))), TRUE)")
\dontshow{stopifnot(}
all.equal(tst2, tst2.)
\dontshow{)}
}
\keyword{manip}
