\name{Interp}
\alias{Interp}
%\alias{Interp.logical}
%\alias{Interp.integer}
%\alias{Interp.numeric}
%\alias{Interp.complex}
%\alias{Interp.raw}
%\alias{Interp.character}
\alias{Interp.default}
\alias{InterpChkArgs}
\alias{InterpChar}
\alias{InterpNum}
\title{
  Interpolate between numbers or numbers of characters 
}
\description{
  Numeric interpolation is defined in the usual way:  

  \code{xOut <- x*(1-proportion) + y*proportion} 
  
  Character interpolation does linear interpolation 
  on the number of characters of \code{x} and 
  \code{y}.  If \code{length(proportion) == 1}, 
  interpolation is done on \code{cumsum(nchar(.))}.  
  If \code{length(proportion) > 1}, interpolation 
  is based on \code{\link{nchar}}.  In either case, 
  the interpolant is rounded to an integer number 
  of characters.  \code{Interp} then returns 
  \code{substring(y, ...)} unless \code{nchar(x)} > 
  \code{nchar(y)}, when it returns 
  \code{substring(x, ...)}.  
  
  Character interpolation is used in two cases:  (1) 
  At least one of \code{x} and \code{y} is 
  character.  (2) At least one of \code{x} and 
  \code{y} is neither logical, integer, numeric, 
  complex nor raw, and \code{class(unclass(.))} is 
  either integer or character.  
  
  In all other cases, numeric interpolation is used.  
  
  NOTE:  This seems to provide a relatively simple 
  default for what most people would want from 
  the six classes of atomic vectors (logical, 
  integer, numeric, complex, raw, and character) 
  and most other classes.  For example, 
  \code{class(unclass(factor))} is integer.  The
  second rule would apply to this converting it to 
  character.  The \code{\link[zoo]{coredata}} of an 
  object of class \code{\link[zoo]{zoo}} could be most
  anything, but this relatively simple rule would 
  deliver what most people want in most case.  An 
  exception would be an object with integer 
  coredata.  To handle this as numeric, a 
  \code{Interp.zoo} function would have to be 
  written.  
}
\usage{
Interp(x, ...)
\method{Interp}{default}(x, y, proportion, 
        argnames=character(3), message0=character(0), ...)
InterpChkArgs(x, y, proportion, 
        argnames=character(3), message0=character(0), ...)
InterpChar(argsChk, ...)        
InterpNum(argsChk, ...)        
}
        
\arguments{
  \item{x, y}{ 
    two vectors of the same class or to be coerced to the 
    same class.  
  }
  \item{proportion}{
    A number or numeric vector assumed to be between 0 and 1.  
  }
  \item{argnames}{
    a character vector of length 3 giving args \code{name.x}, 
    \code{name.y}, and \code{proportion} to pass to 
    \code{\link{compareLengths}} to improve the value  of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{message0}{
    A character string to be passed with \code{argnames} to 
    \code{\link{compareLengths}} to improve the value of 
    any diagnostic message in case lengths are not compatible.  
  }
  \item{argsChk}{
    a list as returned by \code{interpChkArgs}
  }
  \item{\dots}{
    optional arguments for \code{\link{compareLengths}}  
  }
}
\details{
  \code{Interp} is an S3 generic function to allow users to 
  easily modify the behavior to interpolate between special 
  classes of objects.  
  
  \code{Interp} has two basic algorithms for "Numeric" and
  "Character" interpolation.  
  
  The computations begin by calling \code{InterpChkArgs} 
  to dispose quickly of simple cases (e.g, \code{x} or 
  \code{y} \code{\link{missing}} or \code{\link{length}} 0 
  or if \code{proportion} is <= 0 or >= 1 or 
  \code{\link{missing}}).  It returns a list.  
  
  If the list contains a component named "xout", 
  \code{Interp} returns that value with no further 
  computations.  
  
  Otherwise, the list returned by \code{InterpChkArgs} 
  includes components "algorithm", "x", "y", "proportion", 
  "pLength1" (defined below), "raw", and "outclass".  The 
  "algorithm" component must be either "Numeric" or 
  "Character".  That algoritm is then performed as 
  discussed below using arguments "x", "y", and 
  "proportion";  all three will have the same length.  The 
  class of "x" and "y" will match the algorithm.  
  The list component "raw" is logical:  \code{TRUE} if 
  the output will be raw or such that 
  \code{class(unclass(.))} of the output will be raw.  
  In that case, a "Numeric" interpolation will be 
  transformed back into "raw".  "outclass" will either be a 
  list of attributes to apply to the output or NA.  If a 
  list, "xout" will be added as component ".Data" to the 
  list "outclass" and then then processed as 
  \code{do.call('structure', outclass)} to produce the 
  desired output.  
  
  These two basic algorithms ("Numeric" and "Character") are 
  the same if \code{proportion} is missing or not numeric:  
  In that case \code{Interp} throws an error.  
  
  We now consider "Character" first, because it's domain of 
  applicability is easier to describe.  The "Numeric" 
  algorithm is used in all other cases
  
  1.  "CHARACTER" 
  
  * 1.1.  The "CHARACTER" algorithem is used when at 
  least one of \code{x} and \code{y} is neither logical, 
  integer, numeric, complex nor raw and satisfies one 
  of the following two additional conditions:  
  
  ** 1.1.1.  Either \code{x} or \code{y} is character. 
  
  ** 1.1.2.  \code{class(unclass(.))} for at least one 
  of \code{x} and \code{y} is either character or 
  integer.  
  
  NOTE:  The strengths and weaknesses of 1.1.2 can be 
  seen in considering factors and integer vectors of 
  class \code{\link[zoo]{zoo}}:  For both, 
  \code{class(unclass(.))} is integer.  For factors, 
  we want to use \code{as.character(.)}.  For zoo 
  objects with \code{\link[zoo]{coredata}} of class 
  integer, we would want to use numeric interpolation.  
  This is not allowed with the current code but could
  be easily implemented by writing \code{Interp.zoo}.  
    
  * 1.2.  If either \code{x} or \code{y} is missing 
  or has \code{\link{length}} 0, the one that is 
  provided is returned unchanged. 
  
  * 1.3.  Next determine the class of the output.  This 
  depends on whether neither, one or both of \code{x} and 
  \code{y} have one of the six classes of atomic vectors  
  (logical, integer, numeric, complex, raw, character):  
  
  ** 1.3.1.  If both \code{x} and \code{y} have one of 
  the six atomic classes and one is character, return a 
  character object.  
  
  ** 1.3.2.  If only one of \code{x} and \code{y} have
  an atomic class, return an object of the class of the 
  other.  
  
  ** 1.3.3.  If neither of \code{x} nor \code{y} have a 
  basic class, return an object with the class of  
  \code{y}.  
  
  * 1.4.  Set \code{pLength1 <- (length(proportion) == 1)}:  
  
  ** 1.4.1.  If(pLength1) do the linear interpolation on 
  \code{cumsum(nchar(.))}.   
  
  ** 1.4.2.  Else do the linear interpolation on 
  \code{\link{nchar}}.  
  
  * 1.5.  Next check \code{x}, \code{y} and \code{proportion} 
  for comparable lengths:  If all have length 0, return 
  an object of the appropriate class.  Otherwise, call 
  \code{compareLengths(x, proportion)}, 
  \code{compareLengths(y, proportion)}, and 
  \code{compareLengths(x, y)}.  
  
  * 1.6.  Extend \code{x}, \code{y}, and \code{proportion} to 
  the length of the longest using \code{\link{rep}}.  
  
  * 1.7. nchOut <- the number of characters to output using 
  numeric interpolation and rounding the result to integer.  
  
  * 1.8.  Return \code{substring(y, 1, nchOut)} except 
  when the number of characters from \code{x} exceed 
  those from \code{y}, in which case return 
  \code{substring(x, 1, nchOut)}.  [NOTE:  This meets the 
  naive end conditions that the number of characters matches 
  that of \code{x} when \code{proportion} is 0 and matches 
  that of \code{y} when \code{proportion} is 1.  This can be 
  used to "erase" characters moving from one frame to the 
  next in a video.  See the examples.   

  2.  "NUMERIC"
  
  * 2.1.  Confirm that this does NOT satisfy the condition 
  for the "Character" algorithm.  
  
  * 2.2.  If either \code{x} or \code{y} is missing or 
  has \code{\link{length}} 0, return the one provided.  
  
  * 2.3.  Next determine the class of the output.  As for 
  "Character" described in section 1.3, this depends on 
  whether neither, one or both of \code{x} and \code{y} have 
  a basic class other than character (logical, integer, 
  numeric, complex, raw):  
  
  ** 2.3.1.  If \code{proportion} <= 0, return \code{x} 
  unchanged.  If \code{proportion} >= 1, return \code{y} 
  unchanged.  
  
  ** 2.3.2.  If neither \code{x} nor \code{y} has a basic 
  class, return an object of class equal that of \code{y}.
  
  ** 2.3.3.  If exactly one of \code{x} and \code{y} does 
  not have a basic class, return an object of class 
  determined by \code{class(unclass(.))} of the non-basic
  argument.  
  
  ** 2.3.4.  When interpolating between two objects of class 
  raw, convert the interpoland back to class raw.  Do this 
  even when 2.3.2 or 2.3.3 applies and 
  \code{class(unclass(.))} of both \code{x} and \code{y}
  are of class raw.  
  
  * 2.4.  Next check \code{x}, \code{y} and 
  \code{proportion} for comparable lengths:  If all have 
  length 0, return an object of the appropriate class.  
  Otherwise, call \code{compareLengths(x, proportion)}, 
  \code{compareLengths(y, proportion)}, and 
  \code{compareLengths(x, y)}.    
  
  * 2.5.  Compute the desired interpolation and convert it 
  to the required class per step 2.3 above.    
}
\value{
  \code{Interp} returns a vector whose class is described 
  in "* 1.3" and "* 2.3" in "Details" above.  
  
  \code{InterpChkArgs} returns a list or throws an error 
  as described in "Details" above.  
}
%\source{}
\references{
  The \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{
  \emph{Writing R Extensions}} manual (available via 
  \code{help.start()}) lists six different classes of 
  atomic vectors:  \code{\link{logical}}, 
  \code{\link{integer}}, \code{\link{numeric}}, 
  \code{\link{complex}}, \code{\link{raw}} and 
  \code{\link{character}}.  See also Wickham, Hadley (2014) 
  \emph{Advanced R}, especially 
  \href{http://adv-r.had.co.nz/Data-structures.html}{Wickham 
  (2013, section on "Atomic vectors" in the chapter on   
  "Data structures")}.  
}
\author{Spencer Graves}
\seealso{
  \code{\link{classIndex}}
  \code{\link{interpPairs}}
  
  Many other packages have functions with names like "interp", 
  "interp1", and "interpolate".  Some do one-dimensional 
  interpolation.  Others do two-dimensional interpolation.  
  Some offer different kinds of interpolation beyond linear.  
  At least one is a wrapper for \code{\link{approx}}.  
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

# 1.2.  with x but not y:  
# return that vector with a warning
\dontshow{
InterpChkArgs(1:4, p=.5) # $xout 1:4 OK 
}
xN1 <- Interp(1:4, p=.5)
# answer 
xN1. <- 1:4
\dontshow{stopifnot(}
all.equal(xN1, xN1.)
\dontshow{)}

##
## 2.  Single character vector 
##
\dontshow{
argChk.x.3 <- InterpChkArgs(c('a', 'bc', 'def'), 
        p=0.3)
argC.5 <- list(xout=c('a', 'bc', 'def'))
all.equal(argChk.x.3, argC.5)

argChk.x0.3 <- InterpChkArgs(c('a', 'bc', 'def'), 
        character(0), p=0.3)
all.equal(argChk.x0.3, argC.5)

argChk.3 <- InterpChkArgs(c('a', 'bc', 'def'), 
        character(1), p=0.3)
argC.3 <- list(algorithm="Character", 
      x=c("a", "bc", "def"), y=character(3), 
      proportion=rep(.3, 3), pLength1=TRUE, 
      raw=FALSE, outClass=NULL)
all.equal(argChk.3, argC.3)

argChk.3p <- InterpChkArgs(c('a', 'bc', 'def'), 
        character(1), p=rep(0.3, 3))
argC.3p <- list(algorithm="Character", 
      x=c("a", "bc", "def"), y=character(3), 
      proportion=rep(.3, 3), pLength1=FALSE, 
      raw=FALSE, outClass=NULL)
all.equal(argChk.3p, argC.3p)
}

i.5 <- Interp(c('a', 'bc', 'def'), character(0), p=0.3)
# with y = NULL or character(0), 
# Interp returns x 
\dontshow{stopifnot(}
all.equal(i.5, c('a', 'bc', 'def'))
\dontshow{)}

i.5b <- Interp('', c('a', 'bc', 'def'), p=0.3)
# Cumulative characters (length(proportion)=1):  
#     0.3*(total 6 characters) = 1.2 characters
i.5. <- c('a', 'b', '')
\dontshow{stopifnot(}
all.equal(i.5b, i.5.)
\dontshow{)}

##
## 3.  Reverse character example 
##
i.5c <- Interp(c('a', 'bc', 'def'), '', 0.3)
# check:  0.7*(total 6 characers) = 4.2 characters
i.5c. <- c('a', 'bc', 'd')
\dontshow{stopifnot(}
all.equal(i.5c, i.5c.)
\dontshow{)}

##
## 4.  More complicated example
##
xCh <- Interp('', c('Do it', 'with R.'), 
              c(0, .5, .9)) 
# answer 
xCh. <- c('', 'with', 'Do i') 
\dontshow{stopifnot(}
all.equal(xCh, xCh.)
\dontshow{)}
##
## 5.  Still more complicated 
##
xC2 <- Interp(c('a', 'fabulous', 'bug'), 
                  c('bigger or', 'just', 'big'), 
                  c(.3, .3, 1) )
x.y.longer <- c('bigger or', 'fabulous', 'big')
# use y with ties 
# nch smaller        1          4         3
# nch larger         9          8         3
# d.char             8,         4,        0 
# prop              .3,        .7,        1 
# prop*d.char      2.4,       2.8,        0
# smaller+p*d        3,         7,        3
xC2. <- c('big', 'fabulou', 'big')                 
\dontshow{stopifnot(}
all.equal(xC2, xC2.)
\dontshow{)}

##
## 6.  with one NULL 
##
null1 <- Interp(NULL, 1, .3)
\dontshow{stopifnot(}
all.equal(null1, 1)
\dontshow{)}

null2 <- Interp('abc', NULL, .3)
\dontshow{stopifnot(}
all.equal(null2, 'abc')
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
Jan1.1980 <- as.Date('1980-01-01')

Jan1.1972i <- Interp(0, Jan1.1980, .2)
# check 
Jan1.1972 <- as.Date('1972-01-01')

\dontshow{stopifnot(}
all.equal(Jan1.1972, round(Jan1.1972i))
\dontshow{)}

##
## 9.  POSIXct 
##
Jan1.1980c <- as.POSIXct(Jan1.1980)

Jan1.1972ci <- Interp(0, Jan1.1980c, .2)
# check 
Jan1.1972ct <- as.POSIXct(Jan1.1972)

\dontshow{stopifnot(}
abs(difftime(Jan1.1972ct, Jan1.1972ci, 
             units="days"))<0.5
\dontshow{)}
}
\keyword{manip}