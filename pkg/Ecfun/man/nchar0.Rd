\name{nchar0}
\alias{nchar0}
\title{
  Zero characters or NULL 
}
\description{
  Returns TRUE if (is.null(x) || (length(x) == 0) || 
  (max(nchar(x)) == 0)).   
}
\usage{
nchar0(x, ...)
}
\arguments{
  \item{x}{
    a character vector or something that can be coerced to mode character 
  }
  \item{\dots}{
    optional arguments to be passed to \code{\link{nchar}}
  }
}
%\details{}
%\source{ }
\value{
  \code{TRUE} if \code{x} is either \code{NULL} or \code{max(nchar(x))} 
  == 0.  \code{FALSE} otherwise.  
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{nchar}}
}
\examples{
\dontshow{stopifnot(}
all.equal(nchar0(NULL), TRUE)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(nchar0(character(0)), TRUE)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(nchar0(character(3)), TRUE)
\dontshow{)}

\dontshow{stopifnot(}
all.equal(nchar0(c('a', 'c')), FALSE)
\dontshow{)}
}
\keyword{manip}
