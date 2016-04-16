\name{canbeNumeric}
\alias{canbeNumeric}
\title{
  Can a variable reasonably be coerced to numeric?  
}
\description{
  Can \code{\link{seq}} be reasonably applied to 
  \code{x}?  Returns TRUE if yes and FALSE otherwise.  
  
  We'd like to use this with, for example, date-time
  objects in \code{\link{as.Date}} and 
  \code{\link{as.POSIXct}} formats.  However, 
  \code{\link{as.numeric}} of such objects is 
  \code{FALSE}.  Moreover, \code{\link{as.numeric}} 
  of \code{\link{factor}}s is TRUE.  
  
  The current algorithm (which may change in the 
  future) returns TRUE if 
  (\code{\link{mode}}(x) == 'numeric') & 
  (!('levels' \%in\% names(attributes(x)))).  
}
\usage{
canbeNumeric(x)
}
\arguments{
  \item{x}{
    an R object
  }
}
%\details{}
\value{
  A \code{\link{logical}} as described above.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{mode}}
}
%\references{}
\examples{
##
## Examples adapted from "mode"
##
cex4 <- c('letters[1:4]', "as.Date('2014-01-02')", 
  'factor(letters[1:4])', "NULL", "1", "1:1", "1i", 
  "list(1)", "data.frame(x = 1)","pairlist(pi)", 
  "c", "lm", "formals(lm)[[1]]",  "formals(lm)[[2]]",
  "y ~ x","expression((1))[[1]]", "(y ~ x)[[1]]",
  "expression(x <- pi)[[1]][[1]]")
lex4 <- sapply(cex4, function(x) eval(parse(text = x)))
mex4 <- t(sapply(lex4, function(x) 
    c(typeof(x), storage.mode(x), mode(x), canbeNumeric(x))))
dimnames(mex4) <- list(cex4, 
    c("typeof(.)","storage.mode(.)","mode(.)", 'canbeNumeric(x)'))
mex4

# check 
mex. <- as.character(as.logical(c(0, 1, 0, 0, 1, 1, rep(0, 12))))
names(mex.) <- cex4
\dontshow{stopifnot(}
all.equal(mex4[,4], mex.)
\dontshow{)}
}
\keyword{aplot}
