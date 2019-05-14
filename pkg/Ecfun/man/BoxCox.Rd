\name{BoxCox}
\alias{BoxCox}
\alias{invBoxCox}
\title{
  Box-Cox power transformation and its inverse 
}
\description{
  Box and Cox (1964) considered the following family
  of transformations indexed by \code{lambda}:  
  
  \code{w} = \code{(y^lambda-1)/lambda}
  
  = \code{expm1(lambda*log(y))/lambda}, 
  
  with the \code{lambda=0} case defined as 
  \code{log(y)} to make \code{w} continuous in 
  \code{lambda} for constant \code{y}.  
  
  They estimate \code{lambda} assuming \code{w}
  follows a normal distribution.  This raises a
  theoretical problem in that \code{y} must be
  positive, which means that \code{w} must follow 
  a truncated normal distribution conditioned on 
  \code{lambda*w} > \code{(-1)}.  
  
  Bickel and Doksum (1981) removed the restriction 
  to positive \code{y}, i.e., to 
  \code{w} > \code{(-1/lambda)} by modifying 
  the transformaton as follows:  

  \code{w} = 
  
  \code{(sgn(y)*abs(y)^lambda-1)/lambda} 
  if \code{lambda != 0} and 
  
  \code{sgn(y)*log(abs(y))} if \code{lambda = 0}, 
  
  where sgn(y) = 1 if y >= 0 and -1 otherwise.  
  
  NOTE:  \code{sgn(y)} is different from 
  \code{\link[base]{sign}}(y), which is 0 for y = 0.
  A two-argument update to the sign function 
  in the base package has been added to 
  this Ecfun package, so \code{\link{sign}}(y, 1) 
  = sgn(y).  

  If (y<0), this transformation is discontinuous 
  at \code{lambda = 0}.  To see this, we rewrite 
  this as 
  
  \code{w} = 
  \code{(sgn(y)*expm1(lambda*log(abs(y))) + 
  (sgn(y)-1)) / lambda} 
  
  = \code{sgn(y)*(log(abs(y)) + O(lambda) + 
  (sgn(y)-1)/lambda}, 
  
  where 
  \href{https://en.wikipedia.org/wiki/Big_O_notation}{O(lambda)}
  indicates a term that is dominated by a constant 
  times lambda.  
  
  If y<0, this latter term (sgn(y)-1)/lambda = 
  (-2)/lambda and becomes Inf as lambda -> 0.  
  
  In practice, we assume that \code{y} > 0, so this
  distinction has little practical value.  However,
  the \code{BoxCox} function computes the 
  Bickel-Doksum version.  
  
  Box and Cox further noted that proper estimation 
  of \code{lambda} should include the Jacobian of 
  the transformation in the log(likelihood).  
  Doing this can be achieved by rescaling the
  transformation with the \code{n}th root of 
  the Jacobian, which can be written as follows:   
  
  \code{j(y, lambda)} = 
  \code{J(y, lambda)^(1/n)} = 
  \code{GeometricMean(y)^(lambda-1)}.  
  
  With this the rescaled power transformation is 
  as follows:    
  
  \code{z} = \code{(y^lambda-1)/(lambda*j(y, lambda)} if \code{lambda!=0}
  or \code{GeometricMean(y)*log(y)} if \code{lambda==0}.  
  
  In addition to facilitating estimation of 
  \code{lambda}, rescaling has the advantage that 
  the units of \code{z} are the same as the units 
  of \code{y}.    
    
  The ouput has class 'BoxCox', which has 
  attributes that allow the input to be recovered
  using \code{invBoxCox}. The default values of 
  the arguments of \code{invBoxCox} are provided 
  by the corresponding \code{\link{attributes}} 
  of \code{z}.  
}
\usage{
  BoxCox(y, lambda, rescale=TRUE, na.rm=rescale) 
  invBoxCox(z, lambda, sign.y, GeometricMean, rescale)
}
\arguments{
  \item{y}{
    a numeric vector for which the power transform 
    is desired  
  }
  \item{lambda}{
    A numeric vector of length 1 or 2.  The first
    component is the power.  If the second 
    component is provided, \code{y} is replaced 
    by \code{y+lambda[2]}.  
  }
  \item{rescale}{
    logical or numeric.  If logical:  
    
    For \code{BoxCox}, this is \code{TRUE} to 
    return the power transform with rescale, 
    \code{z}, above, and \code{FALSE}
    to return the power transform without the
    \code{n}th root of the Jacobian, \code{w}, 
    above.  This defaults to \code{TRUE}, because
    this will give \code{z} the same units as
    \code{y}.  
    
    For \code{invBoxCox}, this is \code{TRUE} 
    if the input argument \code{z} is assumed to 
    have been rescaled by the \code{n}th root of 
    the Jacobian of the transformation.  This
    defaults to a \code{rescale} attribute of
    \code{z} if present or to \code{TRUE} if 
    absent.  
    
    If numeric, it is assumed to be the geometric
    mean of another set of y values to use with 
    new y's.  
  }
  \item{na.rm}{
    logical:  
    \code{TRUE} to remove \code{NA}s from \code{y}
    before computing the geometric mean.  
    
    \code{FALSE} to compute \code{NA}
    for the geometric mean if \code{any(is.na(y))}.  
    
    NOTE:  If \code{na.rm} = \code{FALSE}, the 
    output will be all \code{NA} if 
    \code{rescale} = \code{TRUE}.  This could 
    produce non useable answers in most cases.  
    To avoid that, the default for \code{na.rm} 
    is \code{TRUE} whenever \code{rescale} = 
    \code{TRUE}.  Conversely, applications using
    \code{na.rm} = \code{FALSE} will likely also 
    want \code{rescale} = \code{FALSE} to avoid
    returning a non-answer in these cases.  This
    explains the default \code{na.rm} = 
    \code{rescale}.  
  }
  \item{z}{
    a numeric vector or an object of class 
    \code{BoxCox} for which the inverse Box-Cox
    transform is desired.  
  }
  \item{sign.y}{
    an optional logical vector giving 
    \code{sign(y-lambda[2])} of the data values 
    that presumably generated \code{z}.  Defaults 
    to an \code{sign.y} attribute of \code{z} or 
    to \code{rep(1, length(z))} if no such 
    attribute is present.  
  }
  \item{GeometricMean}{
    an optional numeric scalar giving the 
    geometric mean of the  data values that 
    presumably generated \code{z}.  Defaults to 
    a \code{GeometricMean} attribute of 
    \code{z} or to 1 if no such attribute is 
    present.  
  }
}
\details{
  Box and Cox (1964) discussed 
  
  \code{w(y, lambda) = (y^lambda - 1)/lambda}. 
  
  They noted that \code{w} is continuous in 
  \code{lambda} with w(y, lambda) = \code{log(y)} 
  if \code{lambda} = 0 (by l'Hopital's rule).  
  
  They also discussed  
  
  \code{z(y, lambda) = (y^lambda - 1)/(lambda*g^(lambda-1))}, 
  
  where \code{g} = the geometric mean of \code{y}.

  They noted that proper estimation of 
  \code{lambda} should include the Jacobian of 
  w(y, lambda) with the likelihood.  They further
  showed that a naive normal likelihood using 
  \code{z(y, lambda)} as the response without a
  Jacobian is equivalent to the normal likelihood
  using \code{w(y, lambda)} adjusted appropriately
  using the Jacobian.  See Box and Cox (1964) or 
  \href{https://en.wikipedia.org/wiki/Power_transform}{the Wikipedia article on "Power transform"}.  
  
  Bickel and Doksum (1981) suggested adding 
  \code{sign(y)} to the transformation, as 
  discussed above. 
  
  NUMERICAL ANALYSIS:  
  
  Consider the Bickel and Doksum version described
  above:  
  
  \code{w} <- \code{(sign(y)*abs(y)^lambda-1)/lambda}
  
  if(any(y==0)), GeometricMean(y) = 0.  This 
  creates a problem with the above math.    

  Let ly = log(abs(y)).  Then with la = lambda, 
  
  \code{w} = code{(sign(y)*exp(la*ly)-1)/la}
  
  = \code{sign(y)*ly(1+(la*ly/2)*(1+(la*ly/3)*(1+(la*ly/4)*(1+O(la*ly)))))
  + (sign(y)-1)/la}
  
  For y>0, the last term is zero.  
  \code{\link[MASS]{boxcox}} ignores cases with 
  y<=0 and uses this formula (ignoring the 
  final O(la*ly)) whenever abs(la) <= eps = 1/50. 
  That form is used here also.  
  
  For \code{invBoxCox} a complementary analysis is 
  as follows:  
  
  abs(y+lambda[2]) = abs(1+la*w)^(1/la)
  
  = exp(log1p(la*w)/la) for abs(la*w)<1 
  
  = w*(1-la*w*((1/2)-la*w*((1/3)-la*w*(1/4-...))))

}
\value{
  \code{BoxCox} returns an object of class 
  \code{BoxCox}, being a numeric vector of the 
  same length as \code{y} with the following 
  optional attributes:  
  
  \itemize{
    \item{lambda}{the value of lambda used in 
      the transformation}
    \item{sign.y}{
      sign(y) (or sign(y-lambda[2]) lambda[2] is
      provided and if any of these quantities are
      negative.  Otherwise, this is omitted and all
      are assumed to be positive.
    }
    \item{rescale}{
      logical:  
      \code{TRUE} if \code{z(y, lambda)} 
      is returned rescaled by \code{g^(lambda-1)}
      with g = the geometric mean of y 
      
      and \code{FALSE} if \code{z(y, lambda)} is not
      so rescaled.  
    }
    \item{GeometricMean}{
      If \code{rescale} is numeric, 
      \code{attr(., 'GeometricMean') <- rescale}.  
      
      Otherwise, \code{attr(., 'GeometricMean')} is 
      the Geometric mean of abs(y) = 
      exp(mean(log(abs(y)))) or of 
      abs(y+lambda[2]) if(length(lambda)>1).  
    }
  } 
  
  \code{invBoxCox} returns a numeric vector,
  reconstructing \code{y} from 
  \code{BoxCox(y, ...)}.  
}
\source{
  Bickel, Peter J., and Doksum, Kjell A. (1981) "An
  analysis of transformation revisited", 
  \emph{Journal of the American Statistical Association}, 
  76 (374):  296-311  
    
  Box, George E. P.; Cox, D. R. (1964). "An analysis
  of transformations", 
  \emph{Journal of the Royal Statistical Society}, 
    Series B 26 (2): 211-252. 

  Box, George E. P.; Cox, D. R. (1982). "An analysis
  of transformations revisited, rebutted", 
  \emph{Journal of the American Statistical Association}, 
  77(377): 209-210.    
}
\references{
  \href{https://en.wikipedia.org/wiki/Power_transform}{Wikipedia, "Power transform"}
}
\seealso{
  \code{\link[MASS]{boxcox}} in the MASS package 
  
  \code{\link[MASS]{quine}} in the MASS package for data used in an example below.  
  
  \code{\link[EnvStats]{boxcox}} and 
  \code{\link[EnvStats]{boxcoxCensored}} in the
  EnvStats package.  
  
  \code{\link[drc]{boxcox.drc}} in the drc package.  
  
  \code{\link[car]{boxCox}} in the car package.  
  
  These other uses all wrap the Box-Cox 
  transformation in something larger and do not 
  give the transformation itself directly.  
}
\examples{
##
## 1.  A simple example to check the two algorithms 
##
Days <- 0:9
bc1 <- BoxCox(Days, c(0.01, 1))
# Taylor expansion used for obs 1:7; expm1 for 8:10 

# check 
GM <- exp(mean(log(abs(Days+1))))

bc0 <- (((Days+1)^0.01)-1)/0.01
bc1. <- (bc0 / (GM^(0.01-1)))  
# log(Days+1) ranges from 0 to 4.4 
# lambda = 0.01 will invoke both the obvious
# algorithm and the alternative assumed to be 
# more accurate for (lambda(log(y)) < 0.02).  
attr(bc1., 'lambda') <- c(0.01, 1)  
attr(bc1., 'rescale') <- TRUE 
attr(bc1., 'GeometricMean') <- GM 
class(bc1.) <- 'BoxCox'

\dontshow{stopifnot(}
all.equal(bc1, bc1.)
\dontshow{)}

##
## 2.  another simple example with lambda=0
##
bc0.4 <- BoxCox(1:5, 0)
GM5 <- prod(1:5)^.2
bc0.4. <- log(1:5)*GM5
attr(bc0.4., 'lambda') <- 0  
attr(bc0.4., 'rescale') <- TRUE 
attr(bc0.4., 'GeometricMean') <- GM5 
class(bc0.4.) <- 'BoxCox'

\dontshow{stopifnot(}
all.equal(bc0.4, bc0.4.)
\dontshow{)}

bc0.4e9 <- BoxCox(1:5, .Machine$double.eps)
bc0.4ex <- log(1:5)*exp(mean(log(1:5)))
\dontshow{stopifnot(}
all.equal(bc0.4ex, as.numeric(bc0.4e9))
\dontshow{)}

# now invert:  

bc0.4i <- invBoxCox(bc0.4.)
\dontshow{stopifnot(}
all.equal(1:5, bc0.4i)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(1:5, invBoxCox(bc0.4e9))
\dontshow{)}

##
## 3.  The "boxcox" function in the MASS package 
##     computes a maximum likelihood estimate with  
##     BoxCox(Days+1, lambda=0.21) 
##     with a 95 percent confidence interval of 
##     approximately (0.08, 0.35)
##
bcDays1 <- BoxCox(MASS::quine$Days, c(0.21, 1))

# check 
GeoMean <- exp(mean(log(abs(MASS::quine$Days+1))))

bcDays1. <- ((((MASS::quine$Days+1)^0.21)-1) / 
               (0.21*GeoMean^(0.21-1)))  
# log(Days+1) ranges from 0 to 4.4 
attr(bcDays1., 'lambda') <- c(0.21, 1)  
attr(bcDays1., 'rescale') <- TRUE 
attr(bcDays1., 'GeometricMean') <- GeoMean 
class(bcDays1.) <- 'BoxCox'

\dontshow{stopifnot(}
all.equal(bcDays1, bcDays1.)
\dontshow{)}

iDays <- invBoxCox(bcDays1)
\dontshow{stopifnot(}
all.equal(iDays, MASS::quine$Days)
\dontshow{)}

##
## 4.  Easily computed example 
##
bc2 <- BoxCox(c(1, 4), 2)

# check 
bc2. <- (c(1, 4)^2-1)/4
attr(bc2., 'lambda') <- 2
attr(bc2., 'rescale') <- TRUE 
attr(bc2., 'GeometricMean') <- 2 
class(bc2.) <- 'BoxCox'

\dontshow{stopifnot(}
all.equal(bc2, bc2.)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(invBoxCox(bc2), c(1, 4))
\dontshow{)}

##
## 5.  plot(BoxCox())
##
y0 <- seq(-2, 2, .1)
z2 <- BoxCox(y0, 2, rescale=FALSE)
plot(y0, z2)

# check 
z2. <- (sign(y0)*y0^2-1)/2

attr(z2., 'lambda') <- 2
attr(z2., 'sign.y') <- sign(y0, 1)
attr(z2., 'rescale') <- FALSE 
attr(z2., 'GeometricMean') <- 0
class(z2.) <- 'BoxCox'

\dontshow{stopifnot(}
all.equal(z2, z2.)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(invBoxCox(z2), y0)
\dontshow{)}

}
\keyword{manip}