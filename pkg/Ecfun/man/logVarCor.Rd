\name{logVarCor}
\alias{logVarCor}
\title{
  Log-diagonal reprentation of a variance matrix 
}
\description{
  Translate a square symmetric matrix with positive 
  diagonal elements into a vector of the logarithms 
  of the diagonal elements with the correlations as 
  an attribute, and vice versa.  
}
\usage{
logVarCor(x, corr, ...)
}
\arguments{
  \item{x}{
    If a matrix, translate into a vector with a 
    "corr" attribute.  
    
    If a vector, translate into a matrix.  
  }
  \item{corr}{
    optional vector of correlations for the 
    \code{\link{lower.tri}} portion of a covariance 
    matrix whose diagonal is \code{exp(x)}.  
    
    Use a "corr" attribute of \code{x} only if 
    this argument is \code{\link{missing}}.  
  }
  \item{...}{
    (not currently used)
  }
}
%\details{}
\value{
  if(length(dim(x))==2) return \code{log(diag(x))} 
  with an attribute "corr" equal to the 
  \code{\link{lower.tri}} of \code{cov2cor(x)}.  
  
  Otherwise, return a covariance matrix from 
  \code{x} as described above.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{log}}
  \code{\link{diag}}
  \code{\link{cov2cor}}
  \code{\link{lower.tri}}
  \code{\link{pdLogChol}} converts a k-dimensional 
  covariance matrix into a vector of length 
  \code{choose(k+1, 2)}.  By contrast, \code{logVarCor} 
  returns a vector of length \code{k} with a "corr" 
  attribute of length \code{choose(k, 2)}.  
}
%\references{}
\examples{
##
## 1.  Trivial 1 x 1 matrix
##
# 1.1.  convert vector to "matrix"
mat1 <- logVarCor(1)
# check
\dontshow{stopifnot(}
all.equal(mat1, matrix(exp(1), 1))
\dontshow{)}          

# 1.2.  Convert 1 x 1 matrix to vector 
lVCd1 <- logVarCor(diag(1))
# check 
lVCd1. <- 0
attr(lVCd1., 'corr') <- numeric(0)
\dontshow{stopifnot(}
all.equal(lVCd1, lVCd1.)
\dontshow{)}

##
## 2.  simple 2 x 2 matrix 
##
# 2.1.  convert 1:2 into a matrix 
lVC2 <- logVarCor(1:2)
# check 
lVC2. <- diag(exp(1:2))
\dontshow{stopifnot(}
all.equal(lVC2, lVC2.)
\dontshow{)}

# 2.2.  Convert a matrix into a vector 
lVC2d <- logVarCor(diag(1:2))
# check 
lVC2d. <- log(1:2)
attr(lVC2d., 'corr') <- 0 
\dontshow{stopifnot(}
all.equal(lVC2d, lVC2d.)
\dontshow{)}

##
## 3.  3-d covariance matrix with nonzero correlations 
##
# 3.1.  Create matrix 
(ex3 <- tcrossprod(matrix(c(rep(1,3), 0:2), 3)))
dimnames(ex3) <- list(letters[1:3], letters[1:3])

# 3.2.  Convert to vector 
(Ex3 <- logVarCor(ex3))

# check 
Ex3. <- log(c(1, 2, 5))
names(Ex3.) <- letters[1:3]
attr(Ex3., 'corr') <- c(1/sqrt(2), 1/sqrt(5), 3/sqrt(10))
\dontshow{stopifnot(}
all.equal(Ex3, Ex3.)
\dontshow{)}

# 3.3.  Convert back to a matrix 
Ex3.2 <- logVarCor(Ex3)
# check 
\dontshow{stopifnot(}
all.equal(ex3, Ex3.2)
\dontshow{)}
}
\keyword{multivariate}
