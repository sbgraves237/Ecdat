\name{sign}
\alias{sign}
\title{
  Sign function with zero option
}
\description{
  \code{sign} returns a vector with the signs of 
  the corresponding elements of \code{x}, being
  1, zero, or -1 if the number is positive, 
  zero or negative, respectively.  
  
  This generalizes the 
  \code{\link[base]{sign}} function in the 
  \code{base} package to allow something 
  other than 0 as the the "sign" of 0.  
}
\usage{
sign(x, zero=0L)
}
\arguments{
  \item{x}{
    a numeric vector for which signs 
    are desired
  }
  \item{zero}{
    an \code{\link{integer}} value to be 
    assigned for x==0.  
  }
}
%\details{}
\value{
  an \code{\link{integer}} vector of the 
  same length as \code{x} assuming 
  values 1, zero and -1, as discussed 
  above.  
}
%\source{}
%\references{}
\seealso{
  \code{\link[base]{sign}} 
}
\examples{
##
## 1.  default
##
sx <- sign((-2):2)

# check 
\dontshow{stopifnot(}
all.equal(sx, base::sign((-2):2))
\dontshow{)}

##
## 2.  with zero = 1
##
s1 <- sign((-2):2, 1)

# check 
\dontshow{stopifnot(}
all.equal(s1, rep(c(-1, 1), c(2,3)))
\dontshow{)}
}
\keyword{manip}