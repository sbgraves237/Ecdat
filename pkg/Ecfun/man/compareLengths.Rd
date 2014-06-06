\name{compareLengths}
\alias{compareLengths}
\title{
  Compare the lengths of two objects  
}
\description{
  Issue a warning or error if the lengths of two objects are not 
  compatible.   
}
\usage{
compareLengths(x, y, 
   name.x=deparse(substitute(x), width.cutoff, nlines=1, ...), 
   name.y=deparse(substitute(y), width.cutoff, nlines=1, ...), 
   message0='', compFun=c('NROW', 'length'), 
   action=c(compatible='', incompatible='warning'), 
   length0=c('compatible', 'incompatible', 'stop'), 
   width.cutoff=20, ...) 
}
\arguments{
  \item{x, y}{
    objects whose lengths are to be compared 
  }
  \item{name.x, name.y}{
    names of x and y to use in a message.  Default = 
    deparse(substitute(.), width.cutoff, nlines=1).  
  }
  \item{message0}{
    character string to be included with name.x and name.y 
    in a message.  
  }
  \item{compFun}{
    function to use in the comparison.  
  }
  \item{action}{
    A character vector of length 2 giving the names of functions 
    to call if the lengths are not equal but are either 
    'compatible' or 'incompatible';  '' means no action.    
  }
  \item{length0}{
    If length(x) or length(y) = 0 (but not both), treat 
    this case as specified by \code{length0}.   
  }
  \item{width.cutoff}{
    \code{width.cutoff} argument to pass to 
    \code{\link{deparse}}.  This gives the maximum number of 
    characters to use in a name in error and warning messages.  
  }
  \item{\dots}{ 
    optional arguments for \code{\link{deparse}} 
  }
}
\details{
  1.  If nchar(name.x) = 0 = nchar(name.y), set name.x <- 'x', 
  name.y <- 'y', and append 'in compareLengths:' to message0 
  for more informative messaging.  
  
  2.  lenx <- do.call(compFun, list(x));  
  leny <- do.call(compFun, list(y))
  
  3.  if(lenx==leny)return(c('equal', ''))
  
  4.  Compatible?  
  
  5.  Compose the message.  
  
  6.  "action", as indicated 
}
\value{
  A character vector of length 2.  The first element is either 'equal', 
  'compatible' or 'incompatible'.  The second element is the message 
  composed.  
}
\author{
  Spencer Graves with help from Duncan Murdoch 
}
\seealso{
  \code{\link{interpChar}}
%  \code{\link{animate}}, \code{\link{interpPairs}}
}
%\references{}
\examples{
##
## 1.  equal 
##
\dontshow{stopifnot(}
all.equal(compareLengths(1:3, 4:6), c("equal", ''))
\dontshow{)}

##
## 2.  compatible 
##
a <- 1:2
b <- letters[1:6]
comp.ab <- compareLengths(a, b, message0='Chk:')
comp.ba <- compareLengths(b, a, message0='Chk:')
# check 
chk.ab <- c('compatible', 
            'Chk: length(b) = 6 is 3 times length(a) = 2')
\dontshow{stopifnot(}
all.equal(comp.ab, chk.ab) 
\dontshow{)}
\dontshow{stopifnot(}
all.equal(comp.ba, chk.ab) 
\dontshow{)}

##
## 3.  incompatible 
##
Z <- LETTERS[1:3]
comp.aZ <- compareLengths(a, Z)
# check 
chk.aZ <- c('incompatible', 
    ' length(Z) = 3 is not a multiple of length(a) = 2')
\dontshow{stopifnot(}
all.equal(comp.aZ, chk.aZ) 
\dontshow{)}
##
## 4.  problems with name.x and name.y 
##
comp.ab2 <- compareLengths(a, b, '', '')
# check 
chk.ab2 <- c('compatible', 
             'in compareLengths: length(y) = 6 is 3 times length(x) = 2')
\dontshow{stopifnot(}
all.equal(comp.ab2, chk.ab2) 
\dontshow{)}
##
## 5.  zeroLength 
##
zeroLen <- compareLengths(logical(0), 1) 
# check 
zeroL <- c('compatible', ' length(logical(0)) = 0')
\dontshow{stopifnot(}
all.equal(zeroLen, zeroL)
\dontshow{)}
}
\keyword{manip}