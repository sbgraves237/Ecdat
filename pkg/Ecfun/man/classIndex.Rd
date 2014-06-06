\name{classIndex}
\alias{classIndex}
\alias{index2class}
\title{
  Convert class to an integer 1-7 and vice versa 
}
\description{
  \code{classIndex} converts the class of \code{x} to an integer:  
  
  \enumerate{
    \item NULL
    \item logical 
    \item integer 
    \item numeric
    \item complex 
    \item character 
    \item other 
  }  
  
  \code{index2class} converts an integer back to the 
  corresponding class.  
}
\usage{
classIndex(x)
index2class(i, otherCharacter=TRUE)
}
\arguments{
  \item{x}{
    an object whose class index is desired.  
  }
  \item{i}{
    an integer to be converted to the name of the 
    corresponding class 
  }
  \item{otherCharacter}{
    logical:  TRUE to convert 7 to "character";  
    FALSE to convert 7 to "other".  
  }
}
%\details{}
\value{
  \code{classIndex} returns an integer between 1 and 7 
  depending on \code{class(x)}.  
  
  \code{index2class} returns a character string for the 
  inverse transformation.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{interpChar}} 
}
%\references{}
\examples{
##
## 1.  classIndex
##
x1 <- classIndex(NULL)
x2 <- classIndex(logical(0))
x3 <- classIndex(integer(1))
x4 <- classIndex(numeric(2))
x5 <- classIndex(complex(3))
x6 <- classIndex(character(4))
x7 <- classIndex(list())

# check 
\dontshow{stopifnot(}
all.equal(c(x1, x2, x3, x4, x5, x6, x7), 1:7)
\dontshow{)}

##
## 2.  index2class 
##
c1 <- index2class(1)
c2 <- index2class(2)
c3 <- index2class(3)
c4 <- index2class(4)
c5 <- index2class(5) 
c6 <- index2class(6) 
c7 <- index2class(7)
c7o <- index2class(7, FALSE)

# check 
\dontshow{stopifnot(}
all.equal(c(c1, c2, c3, c4, c5, c6, c7, c7o), 
          c('NULL', 'logical', 'integer', 'numeric', 
            'complex', 'character', 'character', 'other'))
\dontshow{)}

}
\keyword{manip}
