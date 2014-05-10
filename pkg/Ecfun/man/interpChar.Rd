\name{interpChar}
\alias{interpChar}
\alias{interpChar.list}
\alias{interpChar.default}
\title{
  Interpolate between numbers or numbers of characters 
}
\description{
  For \code{x} and \code{y} numeric:  
  
  \code{xOut <- x*(1-.proportion) + y*.proportion} 
  
  Otherwise, coerce to character and return a 
  \code{\link{substring}} of \code{x} or \code{y} with 
  number of characters interpolating linearly between 
  \code{nchar(x)} and \code{nchar(y)};  see details.  
}
\usage{
interpChar(x, ...)
\method{interpChar}{list}(x, .proportion, 
        argnames=character(3), Source=character(0), ...)
\method{interpChar}{default}(x, y, .proportion, 
        argnames=character(3), Source=character(0), ...)
}
\arguments{
  \item{x}{
    either a vector or a list.  If a list, pass the first 
    two elements as the first two arguments of 
    \code{interpChar.default}.  
  }
  \item{y}{ a vector }
  \item{.proportion}{
    A number or numeric vector assumed to be between 0 and 1.  
  }
  \item{argnames}{
    a character vector of length 3 giving args \code{name.x}, 
    \code{name.y}, and \code{Source} to pass to 
    \code{\link{compareLengths}} to improve the value  of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{Source}{
    A character string to be passed with \code{argnames} to 
    \code{\link{compareLengths}} to improve the value of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{\dots}{
    optional arguments for \code{\link{compareLengths}}  
  }
}
\details{
  0.  \code{x}, \code{y} and \code{.proportion} are first compared
  for compatible lengths using \code{\link{compareLengths}}.  A 
  warning is issued if the lengths are not compatible.  They are 
  then all extended to the same length using \code{\link{rep}}.  
  
  1.  If \code{x} and \code{y} are both numeric, \code{interpChar} 
  returns the standard linear interpolation (described above).  
  
  2.  Otherwise, if a single non-numeric argument is provide with 
  \code{.proportion}, that argument is coerced to character, and 
  linear interpolation is done on the cumulative number of 
  characters.  
  
  3.  If \code{x}, \code{y}, and \code{.proportion} are all 
  provided with at least one of \code{x} and \code{y} not being 
  numeric, the algorithm does linear interpolation on the 
  cumulative difference in the number of characters between 
  \code{x} and \code{y}, returning characters from \code{y}.  
  This meets the standard end conditions that \code{x} is 
  returned when \code{.proportion} is 0 and \code{y} is 
  returned when \code{.proportion} is 1, except that the roles
  of \code{x} and \code{y} are reversed whenever 
  \code{nchar(x)} exceeds \code{nchar(y)}.  This can 
  be used to "erase" characters moving from one frame to 
  the next in a video.  See the examples.   
}
\value{
  A vector:  Numeric if \code{x} and \code{y} are both numeric 
  and character otherwise.  The length = max length of 
  \code{x}, \code{y}, and \code{.proportion}.  
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{interpPairs}}
}
\examples{
##
## 1.  numerics 
## 
# 1.1.  standard 
xNum <- interpChar(1:3, 4:5, (0:3)/4)
# answer 
xN. <- c(1, 2.75, 3.5, 4)
\dontshow{stopifnot(}
all.equal(xNum, xN.)
\dontshow{)}

# 1.2.  list of length 1 with a numeric vector: 
#       return that vector with a warning
xN1 <- interpChar(list(a.0=1:4), .5)
# answer 
xN1. <- 1:4
\dontshow{stopifnot(}
all.equal(xN1, xN1.)
\dontshow{)}

##
## 2.  Single character vector 
##
i.5 <- interpChar(c('a', 'bc', 'def'), .p=0.3)
# check:  0.3*(total 6 characters) = 1.8 characters
i.5. <- c('a', 'b', '')
\dontshow{stopifnot(}
all.equal(i.5, i.5.)
\dontshow{)}

##
## 3.  Reverse character example 
##
i.5c <- interpChar(c('a', 'bc', 'def'), '', 0.3)
# check:  0.7*(total 6 characers) = 4.2 characters
i.5c. <- c('a', 'bc', 'd')
\dontshow{stopifnot(}
all.equal(i.5c, i.5c.)
\dontshow{)}

#  The same thing specified in a list 
i.5d <- interpChar(list(c('a', 'bc', 'def'), ''), 0.3)
\dontshow{stopifnot(}
all.equal(i.5d, i.5c.)
\dontshow{)}

##
## 4.  More complicated example
##
xCh <- interpChar(list(c('Do it', 'with R.')), 
                  c(0, .5, .9)) 
# answer 
xCh. <- c('', 'with', 'Do ') 
# With only one input, it's assumed to be y.  
# It is replicated to length(.proportion), 
# With nchar = 5, 7, 5, cum = 5, 12, 17.  
\dontshow{stopifnot(}
all.equal(xCh, xCh.)
\dontshow{)}
##
## 5.  Still more complicated 
##
xC2 <- interpChar(c('a', 'fabulous', 'bug'), 
                  c('bigger or', 'just', 'big'), 
                  c(.3, .3, 1) )
# answer 
x.y.longer <- c('bigger or', 'fabulous', 'big')
# use y with ties 
# nch smaller        1          4         3
# nch larger         9          8         3
# d.char             8,         4,        0 
# cum characters     8,        12,       12 
# prop              .3,        .7,        1 
# prop*12          3.6,       8.4,       12
# cum.sm             1,         5,        8
# cum.sm+prop*12     5,        13,       20
#   -cum(larger[-1]) 5,         4,        3
xC2. <- c('bigge', 'fabu', 'big')                 
\dontshow{stopifnot(}
all.equal(xC2, xC2.)
\dontshow{)}

}
\keyword{manip}