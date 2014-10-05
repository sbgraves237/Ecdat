\name{missing0}
\alias{missing0}
\title{
  Missing or length 0  
}
\description{
  TRUE if \code{x} is missing or if \code{length(x)} 
  is 0.  
}
\usage{
missing0(x)
}        
\arguments{
  \item{x}{ 
    a formal argument as for \code{\link{missing}}  
  }
}
\details{
  Only makes sense called from within another function
}
\value{
  \code{\link{logical}}:  TRUE if \code{x} is 
  \code{\link{missing}} or if \code{length(x)} is 0.  
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{missing}}
}
\examples{
tstFn <- function(x)missing0(x)
# missing 
\dontshow{stopifnot(}
all.equal(tstFn(), TRUE)
\dontshow{)}

# length 0 
\dontshow{stopifnot(}
all.equal(tstFn(logical()), TRUE)
\dontshow{)}

# supplied 
\dontshow{stopifnot(}
all.equal(tstFn(1), FALSE)
\dontshow{)}

}
\keyword{manip}