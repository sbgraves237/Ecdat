\name{rgrep}
\alias{rgrep}
\title{
  Reverse grep  
}
\description{
  Find which pattern matches x.  
}
\usage{
rgrep(pattern, x)
}
\arguments{
  \item{pattern}{
    a \code{\link{character}} vector of regular expressions 
    to be matched to \code{x}
  }
  \item{x}{
    a \code{\link{character}} string or vector for which a 
    matching regular expression is desired.  
  }
}
\details{
  1.  np <- length(pattern)
  
  2.  g. <- rep(NA, np)
  
  3.  for(i in seq(length=np)){
    g.[i] <- (length(grep(pattern[i], x))>0) 
  }
  
  4.  return(which(g.))
}
\value{
  an \code{\link{integer}} vector of indices 
  of elements of \code{pattern} with a match 
  in \code{x}.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{grep}}
}
%\references{}
\examples{
dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
mm <- model.matrix(~ a + b, dd)

b. <- rgrep(names(dd), colnames(mm)[5])
# check 
\dontshow{stopifnot(}
all.equal(b., 2)
\dontshow{)}
}
\keyword{aplot}
