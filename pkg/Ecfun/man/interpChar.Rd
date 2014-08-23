\name{interpChar}
\alias{interpChar}
\alias{interpChar.list}
\alias{interpChar.default}
\title{
  Interpolate between numbers or numbers of characters 
}
\description{
  For \code{x} and \code{y} logical, integer, numeric, 
  Date or POSIX:  
  
  \code{xOut <- x*(1-.proportion) + y*.proportion} 
  
  Otherwise, coerce to character and return a 
  \code{\link{substring}} of \code{x} or \code{y} with 
  number of characters interpolating linearly between 
  \code{nchar(x)} and \code{nchar(y)};  see details.  
  
  *** NOTE:  This function is currently in flux.  
  The results may not match the documentation 
  and may change in the future.  
  
  The current version does character interpolation on 
  the cumulative number of characters with defaults 
  with only one argument that may not be easy to 
  understand and use.  Proposed:  
  
  old:  interpolate on 
  number of characters in each string with the default
  for a missing argument being character(length(x)) 
  [or character(length(y)) or numeric(length(x)) or ...]
  
  2014-08-08: default with either x or y missing 
  should be to set the other to the one we have, so 
  interpChar becomes a no op -- except that 
  values with .proportion outside ("validProportion" = 
  [0, 1] by default) should be dropped.  
}
\usage{
interpChar(x, ...)
\method{interpChar}{list}(x, .proportion, 
        argnames=character(3), message0=character(0), ...)
\method{interpChar}{default}(x, y, .proportion, 
        argnames=character(3), message0=character(0), ...)
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
    \code{name.y}, and \code{.proportion} to pass to 
    \code{\link{compareLengths}} to improve the value  of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{message0}{
    A character string to be passed with \code{argnames} to 
    \code{\link{compareLengths}} to improve the value of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{\dots}{
    optional arguments for \code{\link{compareLengths}}  
  }
}
\details{
  1.  \code{x}, \code{y} and \code{.proportion} are first compared
  for compatible lengths using \code{\link{compareLengths}}.  A 
  warning is issued if the lengths are not compatible.  They are 
  then all extended to the same length using \code{\link{rep}}.  
  
  2.  If \code{x} and \code{y} are both numeric, \code{interpChar} 
  returns the standard linear interpolation (described above).  
  
  3.  If \code{x}, \code{y}, and \code{.proportion} are all 
  provided with at least one of \code{x} and \code{y} not being 
  numeric  or logical, the algorithm does linear interpolation on 
  the difference in the number of characters between 
  \code{x} and \code{y}.  It returns characters from 
  \code{y} except when \code{nchar(x)} > \code{nchar(y)}, 
  in which case it returns characters from \code{x}.  This 
  meets the end conditions that the number of characters 
  matches that of \code{x} when \code{.proportion} is 0 and 
  matches that of \code{y} when \code{.proportion} is 1.  This 
  can be used to "erase" characters moving from one frame to 
  the next in a video.  See the examples.   
  
  4.  If either \code{x} or \code{y} is missing, it is 
  replaced by a default vector of the same type and length;  
  for example, if \code{y} is missing and \code{x} is numeric, 
  \code{y} = \code{numeric(length(x))}.  (If the one supplied
  is not numeric or logical, it is coerced to character.)  
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
  \code{\link{interpPairs}}, which calls \code{interpChar}
  
  \code{\link{classIndex}}, which is called by 
  \code{interpChar} to help decide the class of the 
  interpoland.  
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
i.5 <- interpChar(list(c('a', 'bc', 'def')), .p=0.3)
# If cumulative characters:  
#        0.3*(total 6 characters) = 1.8 characters
#
# However, the current code does something different, 
# returning "a", "bc", "d" <- like using 1-.p?  
# This is a problem with the defaults with a single 
# argument;  ignore this issue for now.  
# 2014-06-04
i.5. <- c('a', 'b', '')
%\dontshow{stopifnot(}
#all.equal(i.5, i.5.)
%\dontshow{)}

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

##
## 6.  with one NULL 
##
null1 <- interpChar(NULL, 1, 1)
\dontshow{stopifnot(}
all.equal(null1, 1)
\dontshow{)}

null2 <- interpChar('abc', NULL, .3)
\dontshow{stopifnot(}
all.equal(null2, 'ab')
\dontshow{)}
##
## 7.  length=0 
##
log0 <- interpChar(logical(0), 2, .6)
\dontshow{stopifnot(}
all.equal(log0, 1.2)
\dontshow{)}

##
## 8.  Date
##



##
## 9.  POSIXct 
##

}
\keyword{manip}