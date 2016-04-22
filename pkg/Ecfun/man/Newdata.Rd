\name{Newdata}
\alias{Newdata}
\title{
  Create a new data.frame for predict 
}
\description{
  Generate a new \code{\link{data.frame}} or 
  \code{\link{matrix}} from another with column(s) 
  selected by \code{x} adopting \code{n} values in 
  \code{range(data[,x])} and all other columns 
  constant.  

  If \code{\link{canbeNumeric}}(x) is TRUE, the output 
  with has \code{x} adopting \code{n} values in the 
  \code{\link{range}}(x) and all other numeric 
  variables at their \code{\link{median}} and other
  variables at their most common values.  

  If \code{\link{canbeNumeric}}(x) is FALSE, the output 
  with has \code{x} adopting all possible values of 
  \code{x} with all other variables at the same 
  constant values as when \code{\link{canbeNumeric}}(x) 
  is TRUE (and \code{n} is ignored).  If \code{x} has a 
  \code{\link{levels}} attribute, the possible values 
  are defined by that \code{\link{levels}} attribute.  
  Otherwise, it is defined by \code{\link{unique}}(x).  

  This is designed to create a new \code{\link{data.frame}}
  to be used as \code{newdata} for \code{\link{predict}}.  
}
\usage{
Newdata(data, x, n, na.rm=TRUE)
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}} or matrix.  
  }
  \item{x}{
    name of a column of \code{data}.  If \code{NA} or 
    \code{NULL}, select all columns of \code{data}.  
  }
  \item{n}{
    an \code{\link{integer}} vector indicating 
    the number of levels of \code{data[, x]} if 
    \code{canbeNumeric(datat[, x])}.  If 
    \code{canbeNumeric(datat[, x])} is \code{FALSE}, 
    take at most \code{n} of the most popular levels.    
    
    Default is 2 if \code{length(x)} > 1 or if 
    \code{x} is either NA or NULL.  
    
    If n = 1, use the median for 
    \code{canbeNumeric} and the most popular level
    otherwise.  
    
    If n < 1, drop that variable.  
  }
  \item{na.rm}{
    \code{\link{logical}} passed to 
    \code{\link{range}}(x)
  }
}
\details{
  1.  Check data, x.  

  2.  If \code{\link{canbeNumeric}}(x) is TRUE, 
  let \code{xNew} be \code{n} values spanning 
  \code{\link{range}}(x).  Else, let 
  \code{xNew} <- \code{\link{levels}}(x).  
  
  3.  If \code{\link{is.null}}(xNew), set it to 
  \code{\link{sort}}(\code{\link{unique}}(x)).  
  
  4.  let newDat <- data[rep(1, n), ], and 
  replace \code{x} by xNew.  

  5.  otherVars <- colnames(data) != x

  6.  for(x2 in otherVars){replace newDat[, x2]:  
  If \code{\link{canbeNumeric}}(x2) is TRUE, 
  use \code{\link{median}}(x2).  Otherwise, 
  use its (first) most common value.}  
}
\value{
  A \code{\link{data.frame}} with \code{n}
  rows and columns matching those of 
  \code{data}, as described above.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{predict.lm}}
}
%\references{}
\examples{
##
## 1.  A reasonable test with numerics, dates, 
##     an ordered factor and character variables
##
xDate <- as.Date('2001-02-03')+1:4
tstDF <- data.frame(x1=1:4, xDate=xDate, 
  xD2=as.POSIXct(xDate), 
  sex=ordered(c('M', 'F', 'M', 'F')), 
  huh=letters[c(1:3, 3)], stringsAsFactors=FALSE)

newDat <- Newdata(tstDF, 'xDate', n=5)

# check
newD <- data.frame(x1=2.5, 
  xDate=xDate[1]+seq(0, 3, length=5), 
  xD2=as.POSIXct(xDate[2]+0.5), 
  sex=ordered(c('M', 'F', 'M', 'F'))[2], 
  huh=letters[3], stringsAsFactors=FALSE)
attr(newD, 'out.attrs') <- attr(newDat, 'out.attrs')
\dontshow{stopifnot(}
all.equal(newDat, newD)
\dontshow{)}

##
## 2.  Test with only one column 
##
newDat1 <- Newdata(tstDF[, 2, drop=FALSE], 'xDate', n=5)

# check 
newDat1. <- newD[, 2, drop=FALSE]
attr(newDat1., 'out.attrs') <- attr(newDat1, 'out.attrs')
\dontshow{stopifnot(}
all.equal(newDat1, newDat1.)
\dontshow{)}

##
## 3.  Test with a factor 
##
newSex <- Newdata(tstDF, 'sex')

# check 
newS <- with(tstDF, data.frame(
  x1=2.5, xDate=xDate[1]+1.5, 
  xD2=as.POSIXct(xDate[1]+1.5), 
  sex=ordered(c('M', 'F'))[2:1], 
  huh=letters[3], stringsAsFactors=FALSE) )
attr(newS, 'out.attrs') <- attr(newSex, 'out.attrs')
\dontshow{stopifnot(}
all.equal(newSex, newS)
\dontshow{)}

##
## 4.  Test with an integer column number 
##
newDat2 <- Newdata(tstDF, 2, n=5)

# check 
\dontshow{stopifnot(}
all.equal(newDat2, newD)
\dontshow{)}

##
## 5.  Test with all
##
NewAll <- Newdata(tstDF)

# check 
tstLvls <- as.list(tstDF[c(1, 4), ])
tstLvls$sex <- tstDF$sex[2:1]
tstLvls$huh <- letters[c(3, 1)]
tstLvls$stringsAsFactors <- FALSE

NewA. <- do.call(expand.grid, tstLvls)
attr(NewA., 'out.attrs') <- attr(NewAll, 'out.attrs')
\dontshow{stopifnot(}
all.equal(NewAll, NewA.)
\dontshow{)}
}
\keyword{manip}
