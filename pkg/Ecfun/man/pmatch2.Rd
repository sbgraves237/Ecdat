\name{pmatch2}
\alias{pmatch2}
\title{
  Value matching or partial matching
}
\description{
  \code{pmatch2} returns a list of the positions of matches or 
  partial matches of \code{x} in \code{table}.  
  
  This does sloppy matching to find "Peter" to match "Pete" 
  only if "Pete" is not in \code{table}, and we want 
  "John Peter" if neither "Pete" nor "Peter" are in 
  \code{table}.  
}
\usage{
pmatch2(x, table)
}
\arguments{
  \item{x}{
    the values to be matched
  }
  \item{table}{
    the values to be matched against
  }
}
\details{
  1.  nx <- length(x); out <- vector(nx, "list"); 
  names(out) <- x
  
  2.  for(ix in seq(length=nx)):  
  
  3.  xi <- which(x[ix] \%in\% table)
  
  4.  if(length(xi)<1)
  xi <- grep(paste0('^', x[ix]), table).  
  
  5.  if(length(xi)<1)xi <- grep(x[ix], table).  
  
  6.  out[[ix]] <- xi 
}
\value{
  A list of integer vectors indicating the positions in 
  \code{table} matching each element of \code{x}
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{match}}
  \code{\link{pmatch}}
  \code{\link{grep}}
  \code{\link{matchName}}
}
%\references{}
\examples{
##
## 1.  common examples 
##
x2match <- c('Pete', 'Peter', 'Ma', 'Mo', 'Paul', 
             'Cardenas')

tbl <- c('Peter', 'Mary', 'Martha', 'John Paul', 'Peter', 
         'Cardenas', 'Cardenas') 

x2mtchd <- pmatch2(x2match, tbl) 

# answer
x2mtchd. <- list(Pete=c(1, 5), Peter=c(1, 5), Ma=2:3, 
    Mo=integer(0), Paul=4, Cardenas=6:7)
\dontshow{stopifnot(}
all.equal(x2mtchd, x2mtchd.)
\dontshow{)}
##
## 2.  strange cases that caused errors and are now warnings
##
huh <- pmatch2("(7", tbl)

# answer 
huh. <- list("(7"=integer(0))
\dontshow{stopifnot(}
all.equal(huh, huh.)
\dontshow{)}

}
\keyword{manip}
