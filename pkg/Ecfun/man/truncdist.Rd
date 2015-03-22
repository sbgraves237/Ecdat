\name{truncdist}
\alias{truncdist}
\alias{dtruncdist}
\alias{ptruncdist}
\alias{qtruncdist}
\alias{rtruncdist}
\title{
  Truncated distribution 
}
\description{
  The cumulative distribution function for a truncated 
  distribution is \code{0} for \code{x <= truncmin},
  \code{1} for \code{truncmax < x}, and in between is
  as follows:  
  
  \code{(pdist(x, ...) - pdist(truncmin, ...)) / 
          (pdist(truncmax, ...) - pdist(truncmin, ...))}
        
  The density, quantile, and random number generation 
  functions are similarly defined from this.  
}
\usage{
dtruncdist(x, ..., dist='norm', truncmin=-Inf, truncmax=Inf) 
ptruncdist(q, ..., dist='norm', truncmin=-Inf, truncmax=Inf) 
qtruncdist(p, ..., dist='norm', truncmin=-Inf, truncmax=Inf) 
rtruncdist(n, ..., dist='norm', truncmin=-Inf, truncmax=Inf) 
}
\arguments{
  \item{x, q}{ 
    numeric vector of quantiles 
  }
  \item{p}{
    numeric vector of probabilities  
  }
  \item{n}{
    number of observations. If \code{length(n) > 1}, the 
    length is taken to be the number required.
  }
  \item{\dots}{
    other arguments to be passed to the corresponding 
    function for the indicated \code{dist}
  }
  \item{dist}{
    Standard \code{R} name for the family of functions
    for the desired distribution.  By default, this is 
    "norm", so the corresponding function for 
    \code{dtruncdist} is \code{dnorm}, the corresponding 
    function for \code{ptrunctist} is \code{pnorm}, etc.  
  }
  \item{truncmin, truncmax}{
    lower and upper truncation points, respectively.  
  }
}
\details{
  NOTE:  Truncation is different from "censoring", where it's 
  known that an observation lies between certain limits; it's just 
  not known exactly where it lies between those limits.  
  
  By contrast, with a truncated distrubion, events below 
  \code{truncmin} and above \code{truncmax} may exist but are 
  not observed.  Thus, it's not known how many events occur  
  outside the given range, \code{truncmin} to 
  \code{truncmax}, if any.  Given data believed to come from 
  a truncated distribution, estimating the parameters provide 
  a means of estimating the number of unobserved events, 
  assuming a particular form for their distribution.  
  
  1.  Setup
  
  dots <- list(...)

  2.  For \code{dtruncdist}, return 0 for all \code{x} outside
  \code{truncmin} and \code{truncmax}.  For all others, 
  compute as follows:  
  
  dots$x <- truncmin
  ddist <- paste0('d', dist)
  pdist <- paste0('p', dist)
  p.min <- do.call(pdist, dots)
  dots$x <- truncmax
  p.max <- do.call(pdist, dots)
  dots$x <- x
  dx <- do.call(ddist, dots)
  
  return(dx / (p.max-p.min))
  
  NOTE:  Adjustments must be made if 'log' appears in 
  \code{names(dots)}
  
  3.  The computations for \code{ptruncdist} are similar.  
  
  4.  The computations for \code{qtruncdist} are 
  complementary.  
  
  5.  For \code{rtruncdist}, use 
  \code{qtruncdist(runif(n), ...)}.  
}
\value{
  \code{dtruncdist} gives the density, \code{ptruncdist}
  gives the distribution function, \code{qtruncdist} gives 
  the quantile function, and \code{rtruncdist} generates 
  random deviates. 
  
  The length of the result is determined by \code{n} for 
  \code{rtruncdist} and is the maximum of the lengths of 
  the numerical arguments for the other functions. 
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{Distributions}}
  \code{\link{Normal}}
}
\examples{
##
## 1.  dtruncdist
##
#  1.1.  Normal 
dx <- dtruncdist(1:4)

# check 
\dontshow{stopifnot(}
all.equal(dx, dnorm(1:4))
\dontshow{)}

#  1.2.  Truncated normal between 0 and 1
dx01 <- dtruncdist(seq(-1, 2, .5), truncmin=0, truncmax=1)

# check 
dx01. <- c(0, 0, 0, dnorm(c(.5, 1))/(pnorm(1)-pnorm(0)), 
           0, 0)
\dontshow{stopifnot(}
all.equal(dx01, dx01.)
\dontshow{)}

#  1.3.  lognormal meanlog=log(100), sdlog = 2, truncmin=500 
x10 <- 10^(0:9)
dx10 <- dtruncdist(x10, log(100), 2, dist='lnorm', 
                   truncmin=500)
                  
# check 
dx10. <- (dtruncdist(log(x10), log(100), 2,  
                    truncmin=log(500)) / x10) 
\dontshow{stopifnot(}
all.equal(dx10, dx10.)
\dontshow{)}

#  1.4.  log density of the previous example 
dx10log <- dtruncdist(x10, log(100), 2, log=TRUE, 
                  dist='lnorm', truncmin=500)
\dontshow{stopifnot(}
all.equal(dx10log, log(dx10))
\dontshow{)}

#  1.5.  Poisson without 0.  

dPois0.9 <-dtruncdist(0:9, lambda=1, dist='pois', truncmin=0) 

# check 
dP0.9 <- c(0, dpois(1:9, lambda=1)/ppois(0, lambda=1, lower.tail=FALSE))
\dontshow{stopifnot(}
all.equal(dPois0.9, dP0.9)
\dontshow{)}

##
## 2.  ptruncdist
##
#  2.1.  Normal 
px <- ptruncdist(1:4)

# check 
\dontshow{stopifnot(}
all.equal(px, pnorm(1:4))
\dontshow{)}

#  2.2.  Truncated normal between 0 and 1 
px01 <- ptruncdist(seq(-1, 2, .5), truncmin=0, truncmax=1)

# check 
px01. <- c(0, 0, (pnorm(c(0, .5, 1)) - pnorm(0))
                     /(pnorm(1)-pnorm(0)), 1, 1)
\dontshow{stopifnot(}
all.equal(px01, px01.)
\dontshow{)}

#  2.3.  lognormal meanlog=log(100), sdlog = 2, truncmin=500 
x10 <- 10^(0:9)
px10 <- ptruncdist(x10, log(100), 2, dist='lnorm', 
                  truncmin=500)
                  
# check 
px10. <- (ptruncdist(log(x10), log(100), 2,  
                     truncmin=log(500))) 
\dontshow{stopifnot(}
all.equal(px10, px10.)
\dontshow{)}

#  2.4.  log of the previous probabilities 
px10log <- ptruncdist(x10, log(100), 2, log=TRUE, 
                  dist='lnorm', truncmin=500)
\dontshow{stopifnot(}
all.equal(px10log, log(px10))
\dontshow{)}

##
## 3.  qtruncdist
##
#  3.1.  Normal 
qx <- qtruncdist(seq(0, 1, .2))

# check 
\dontshow{stopifnot(}
all.equal(qx, qnorm(seq(0, 1, .2)))
\dontshow{)}

#  3.2.  Normal truncated outside (0, 1)
qx01 <- qtruncdist(seq(0, 1, .2), truncmin=0, truncmax=1)

# check 
pxmin <- pnorm(0)
pxmax <- pnorm(1)
unp <- (pxmin + seq(0, 1, .2)*(pxmax-pxmin))
qx01. <- qnorm(unp)
\dontshow{stopifnot(}
all.equal(qx01, qx01.)
\dontshow{)}

#  3.3.  lognormal meanlog=log(100), sdlog=2, truncmin=500
qlx10 <- qtruncdist(seq(0, 1, .2), log(100), 2, 
                   dist='lnorm', truncmin=500)
                  
# check 
plxmin <- plnorm(500, log(100), 2)
unp. <- (plxmin + seq(0, 1, .2)*(1-plxmin))

qlx10. <- qlnorm(unp., log(100), 2)
\dontshow{stopifnot(}
all.equal(qlx10, qlx10.)
\dontshow{)}

#  3.4.  previous example with log probabilities 
qlx10l <- qtruncdist(log(seq(0, 1, .2)), log(100), 2, 
                   log.p=TRUE, dist='lnorm', truncmin=500)

# check 
\dontshow{stopifnot(}
all.equal(qlx10, qlx10l)
\dontshow{)}

## 
## 4.  rtruncdist 
##
#  4.1.  Normal 
set.seed(1)
rx <- rtruncdist(9)

# check 
set.seed(1)
\dontshow{stopifnot(}
all.equal(rx[1], rnorm(1))
\dontshow{)}
# Only the first observation matches;  check that.  

#  4.2.  Normal truncated outside (0, 1)
set.seed(1)
rx01 <- rtruncdist(9, truncmin=0, truncmax=1)

# check 
pxmin <- pnorm(0)
pxmax <- pnorm(1)
set.seed(1)
rnp <- (pxmin + runif(9)*(pxmax-pxmin))
rx01. <- qnorm(rnp)
\dontshow{stopifnot(}
all.equal(rx01, rx01.)
\dontshow{)}

#  4.3.  lognormal meanlog=log(100), sdlog=2, truncmin=500
set.seed(1)
rlx10 <- rtruncdist(9, log(100), 2, 
                   dist='lnorm', truncmin=500)
                  
# check 
plxmin <- plnorm(500, log(100), 2)
set.seed(1)
rnp. <- (plxmin + runif(9)*(1-plxmin))

rlx10. <- qlnorm(rnp., log(100), 2)
\dontshow{stopifnot(}
all.equal(rlx10, rlx10.)
\dontshow{)}

}
\keyword{distribution}