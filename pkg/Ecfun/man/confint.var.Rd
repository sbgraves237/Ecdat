\name{confint.var}
\alias{confint.var}
\alias{confint.sd}
\title{
  Confidence interval for sample variance or 
  standard deviation 
}
\description{
  Computes the standard normal (i.e., chi-square)
  confidence intervals for a sample variance or 
  standard deviation.    
}
\usage{
\method{confint}{var}(object, parm, level=0.95, ...) 
\method{confint}{sd}(object, parm, level=0.95, ...) 
}
\arguments{
  \item{object}{
    a numeric vector possibly with a "df" or 
    "df.residuals" attribute assumed to 
    represent a sample variance, possibly 
    computed as root mean square of residuals
    from a model.  
  }
  \item{parm}{
    degrees of freedom in the estimated 
    variance or standard deviation.  
  }
  \item{level}{
    the confidence level required
  }
  \item{\dots}{
    optional arguments not used.  
  }
}
\details{
  1.  If \code{object} is not numeric, throw
  an error.  
  
  2.  If \code{parm} is missing, look for an 
  attribute of \code{object} starting with "df".  
  If present, use that for \code{parm}.  If
  \code{parm} is absent or not numeric, throw 
  an error.  
  
  3.  replicate \code{object}, \code{parm}, and 
  \code{level} to the same length.  Issue a warning 
  if the longest is not a multiple of the others.  
  
  4.  alph2 <- (1-level)/2 
  
  5.  Qntls <- cbind(lower=qchisq(alph2, parm, lower=FALSE), 
        upper=qchisq(alph2, parm)) 
        
  6.  CI <- (object*parm/Qntls)
  
  7.  attr(CI, 'level') <- Level
  
  7.  return(CI)
}
\value{
  a matrix with columns "lower" and "upper", 
  \code{nrow} = the longest of the lengths 
  of \code{object}, \code{parm}, and \code{level}, 
  and an attribute "level".  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{cor.test}}, 
  \code{\link[DescTools]{VarCI}}
}
\references{
  \href{https://en.wikipedia.org/wiki/Standard_deviation}{
    Wikipedia, "Standard deviation"}, accessed 
    2016-07-06. 
}
\examples{
##
## 1.  simple examples 
##
(CI.v <- confint.var(c(1,1,4), c(1, 9, 9))) 
(CI.s <- confint.sd(c(1,1,2), c(1, 9, 9))) 

# Compare with the examples on Wikipedia
\dontshow{stopifnot(}
all.equal(CI.s, sqrt(CI.v))
\dontshow{)}

WikipEx <- t(matrix(c(0.45, 31.9, 0.69, 1.83, 1.38, 3.66), 
                  nrow=2))
colnames(WikipEx) <- c('lower', 'upper')

(dCI <- (CI.s-WikipEx))
#Confirm within 2-digit roundoff
\dontshow{stopifnot(}
max(abs(dCI))<0.0102
\dontshow{)}

##
## 2.  test df attributes
##
v <- c(1,1,4)
attr(v, 'df.') <- c(1, 9, 9)
class(v) <- 'var'
vCI <- confint(v)

# check 
\dontshow{stopifnot(}
all.equal(vCI, CI.v)
\dontshow{)}

s <- sqrt(v)
class(s) <- 'sd'
sCI <- confint(s)

# check 
\dontshow{stopifnot(}
all.equal(sCI, CI.s)
\dontshow{)}

}
\keyword{htest}
