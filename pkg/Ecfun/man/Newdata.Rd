\name{Newdata}
\alias{Newdata}
\title{
  Create a new data.frame varying one numeric variable 
}
\description{
  Generate a new \code{\link{data.frame}} from another with 
  one numeric variable \code{x} adopting \code{n} values 
  in the \code{\link{range}}(x) and all other numeric 
  variables at their \code{\link{median}} and 
  \code{\link{factor}}s at their most common values.  
  
  This is designed to create a new \code{\link{data.frame}}
  to be used as input to \code{\link{predict}}.  
}
\usage{
Newdata(data, x, n=31, na.rm=TRUE)
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}}
  }
  \item{x}{
    name of a column of \code{data}
  }
  \item{n}{
    a \code{\link{integer}} indicating the number 
    of equally spaced values of \code{x} to 
    generate.  
  }
  \item{na.rm}{
    \code{\link{logical}} passed to 
    \code{\link{range}}(x)
  }
}
\details{
  1.  x.rng <- range(data[, x], na.rm=na.rm)

  2.  newDat <- data[rep(1, n), ]
  
  3.  newDat[, x] <- seq(x.rng[1], x.rng[2], length=n)
  
  4.  vars <- names(data)
  
  5.  otherVars <- vars[!(vars == x)]
  
  6.  for(x2 in otherVars){replace newDat[, x2]:  
  if x2 is character or has a 'levels' attribute:  
    first most common value 
  else if as.numeric(x2) does not create new NAs, 
    median(x2) else most common value.  
    }
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
\dontshow{stopifnot(}
all.equal(newDat, newD)
\dontshow{)}
##
## 2.  Test with only one column 
##
newDat1 <- Newdata(tstDF[, 2, drop=FALSE], 'xDate', n=5)
\dontshow{stopifnot(}
all.equal(newDat1, newD[, 2, drop=FALSE])
\dontshow{)}
##
## 3.  Test with an integer column number 
##
newDat2 <- Newdata(tstDF, 2, n=5)
\dontshow{stopifnot(}
all.equal(newDat2, newD)
\dontshow{)}


}
\keyword{aplot}
