\name{createMessage}
\alias{createMessage}
\title{
  Compose a message as a single substring from a character vector 
}
\description{
  This is a utility function to make it easier to automatically 
  compose informative error and warning messages without 
  using too many characters.  
}
\usage{
createMessage(x, width.cutoff=45, default='x', collapse='; ', 
              endchars='...')
}
\arguments{
  \item{x}{
    input for \code{\link{paste}}
  }
  \item{width.cutoff}{
    maximum number of characters from x to return 
    in a single string.  This differs from the \code{width.cutoff}
    argument in \code{\link{deparse}} in that the output include
    here considers \code{endchars}, not part of 
    \code{\link{deparse}}.  
  }
  \item{default}{
    character string to return if nchar(x) = 0.  
  }
  \item{collapse}{
    \code{collapse} argument for \code{\link{paste}} 
  }
  \item{endchars}{
    a character string to indicate that part of the 
    input string(s) was truncated.      
  }
}
\details{
  x. <- paste(..., collapse='; ')
  nchx <- nchar(x.)
  maxch <- (maxchar-nchar(endchar))
  if(nchx>maxch){
    x2 <- substring(x., 1, maxch)
    x. <- paste0(x2, endchar)
  }
}
%\source{ }
\value{
  a character string with at most \code{width.cutoff} 
  characters.  
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{paste}}
  \code{\link{substr}}
  \code{\link{nchar}}
}
\examples{
##
## 1.  typical use 
##
tstVec <- c('Now', 'is', 'the', 'time')
msg <- createMessage(tstVec, 9, collapse=':', 
                     endchars='//')
\dontshow{stopifnot(}
all.equal(msg, 'Now:is://')
\dontshow{)}
##
## 2.  in a function 
##
tstFn <- function(cl)createMessage(deparse(cl), 9)
Cl <- quote(plot(1:3, y=4:6, col='red', main='Title'))
msg0 <- tstFn(Cl)
# check 
msg. <- 'plot(1...'
\dontshow{stopifnot(}
all.equal(msg0, msg.)
\dontshow{)}

##
## 3.  default 
##
y <- createMessage(character(3), default='y') 
\dontshow{stopifnot(}
all.equal(y, 'y')
\dontshow{)}

}
\keyword{manip}
