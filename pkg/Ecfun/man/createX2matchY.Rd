\name{createX2matchY}
\alias{createX2matchY}
\title{
  Create X to match Y 
}
\description{
  Return a default object of class 
  \code{index2class(max(classIndex(x), classIndex(y)))} 
  and length = length(y).  
  
  For example, suppose \code{class(x)} == 'numeric', for which
  \code{classIndex} = 4.  If \code{class(y)} = 'integer', then 
  an object of class 'numeric' is returned.  However, if 
  \code{class(y)} = 'character', then an object of class 
  'character' is returned.    
}
\usage{
createX2matchY(x, y)
}
\arguments{
  \item{x, y}{
    objects of possibly different classes and lengths.  
  }
}
%\details{}
\value{
  A vector of the same length as \code{y} whose class is 
  \code{index2class(max(classIndex(x), classIndex(y)))}.  
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{interpPairs}}
}
\examples{
##
## 1.  NULL 
## -
null <- createX2matchY(NULL, NULL)
# check 
\dontshow{stopifnot(}
all.equal(null, NULL)
\dontshow{)}
##
## 2.  logical 
##
lgcl3 <- createX2matchY(NULL, 
           c(FALSE, TRUE, FALSE))

# check 
\dontshow{stopifnot(}
all.equal(lgcl3, logical(3))
\dontshow{)}

##
## 3.  integer
##
int3 <- createX2matchY(integer(0), 
           c(FALSE, TRUE, FALSE))
# check 
\dontshow{stopifnot(}
all.equal(int3, integer(3))
\dontshow{)}

##
## 4.  list -> character
##
ch3 <- createX2matchY(integer(0), 
         list(a=1, b=2, c=3))
# check 
\dontshow{stopifnot(}
all.equal(ch3, character(3))
\dontshow{)}
}
\keyword{manip}