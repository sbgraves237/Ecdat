\name{classIndex}
\alias{classIndex}
\alias{index2class}
\title{
  Convert class to an integer 1-8 and vice versa 
}
\description{
  \code{classIndex} converts the class of \code{x} to an integer:  
  
  \enumerate{
    \item NULL
    \item logical 
    \item integer 
    \item numeric
    \item complex 
    \item raw 
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
    logical:  TRUE to convert 8 to "character";  
    FALSE to convert 8 to "other".  
  }
}
\details{
  The \href{https://cran.r-project.org/doc/manuals/r-release/R-exts.html}{
  Writing R Extensions} lists six different kinds of "atomic 
  vectors":  logical, integer, numeric, complex, character,
  and raw:  See also 
  \href{http://adv-r.had.co.nz/Data-structures.html}{Wickham 
  (2013, section on "Atomic vectors" in the chapter on   
  "Data structures")}.  These form a standard heirarchy, 
  except for "raw", in that standard operations combining 
  objects with different atomic classes will create an 
  object of the higher class.  For example, \code{TRUE + 
  2 + pi} returns a numeric object ((approximately 
  6.141593).  Similarly, \code{paste(1, 'a')} returns
  the character string "1 a". 
  
  For "interpolation", we might expect users interpolating 
  between objects of class "raw" (i.e., bytes) might most 
  likely prefer "Numeric" to "Character" interpolation, 
  coerced back to type "raw".  
  
  The index numbers for the classes run from 1 to 8 to 
  make it easy to convert them back from integers to 
  character strings.  
}
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
\references{
Wickham, Hadley (2014) \emph{Advanced R}, 
especially \href{http://adv-r.had.co.nz/Data-structures.html}{Wickham 
  (2013, section on "Atomic vectors" in the chapter on   
  "Data structures")}.  
}
\examples{
##
## 1.  classIndex
##
x1 <- classIndex(NULL)
x2 <- classIndex(logical(0))
x3 <- classIndex(integer(1))
x4 <- classIndex(numeric(2))
x5 <- classIndex(complex(3))
x6 <- classIndex(raw(4))
x7 <- classIndex(character(5))
x8 <- classIndex(list())

# check 
\dontshow{stopifnot(}
all.equal(c(x1, x2, x3, x4, x5, x6, x7, x8), 1:8)
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
c8 <- index2class(8)
c8o <- index2class(8, FALSE)

# check 
\dontshow{stopifnot(}
all.equal(c(c1, c2, c3, c4, c5, c6, c7, c8, c8o), 
          c('NULL', 'logical', 'integer', 'numeric', 
            'complex', 'raw', 'character', 'character', 
            'other'))
\dontshow{)}

}
\keyword{manip}
