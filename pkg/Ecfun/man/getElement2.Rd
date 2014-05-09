\name{getElement2}
\alias{getElement2}
\title{
  Extract a named element from an object with a default
}
\description{
  Get element \code{name} of \code{object}.  If \code{object} does not
  have an element \code{name}, return \code{default}.

  If the \code{name} element of \code{object} is \code{NULL} the result
  depends on \code{warn.NULL}:  If \code{TRUE}, issue a warning and
  return \code{default}.  Otherwise, return \code{NULL}
}
\usage{
getElement2(object, name=1, default=NA, warn.NULL=TRUE, 
            envir=list(), returnName)
}
\arguments{
  \item{object}{
    object from which to extract component \code{name}.
  }
  \item{name}{
    Name or index of the element to extract
  }
  \item{default}{
    default value if \code{name} is not part of \code{object}.
  }  
  \item{warn.NULL}{
    logical to decide how to treat cases where \code{object} has a
    component \code{name}:  If \code{TRUE}, return \code{default} with 
    a warning.  Otherwise, return \code{NULL}.
  }
  \item{envir}{
    Supplemental list beyond \code{object} in which to look for names 
    in case \code{object[[name]]} is a language object that must be 
    evaluated.
  }
  \item{returnName}{
    logical:  \code{TRUE} to return \code{\link{as.character}} of any 
    \code{\link{name}} found as an element of \code{object}.  
    
    FALSE to \code{\link{eval}} any \code{\link{name}} found in 
    the environment of  \code{object}.  
    
    Default = \code{TRUE} if \code{name} == 1 or a character string 
    matching the name of the first element of \code{object}.  
  }
}
\details{
  1.  If is.numeric(name) In <- (1 <= name <= length(object)) 
  
  2.  else In <- if(name \%in\% names(object))
  
  3.  El <- if(In) object[[name]] else default 
  
  4.  warn.NULL? 
  
  5.  if(returnName) return(as.character(El)) else 
  return(eval(El, envir=object))
}
\value{
  an object of the form of \code{object[[name]]};  if \code{object} does
  not have an element or slot \code{name}, return \code{default}.
}
\author{
  Spencer Graves with help from Marc Schwartz and Hadley Wickham
}
\seealso{
  \code{\link{getElement}}, which also can return slots from S4 objects.  
}
%\references{}
\examples{
##
## 1.  name in object, return 
##
e1 <- getElement2(list(ab=1), 'ab', 2) # 1
# check 
\dontshow{stopifnot(}
all.equal(e1, 1)
\dontshow{)}

##
## 2.  name not in object, return default
##
eNA <- getElement2(list(), 'ab') # default default = NA
# check 
\dontshow{stopifnot(}
all.equal(eNA, NA)
\dontshow{)}

e0 <- getElement2(list(), 'ab', 2) # name not in object
\dontshow{stopifnot(}
all.equal(e0, 2)
\dontshow{)}

e2 <- getElement2(list(ab=1), 'a', 2) # partial matching not used 
\dontshow{stopifnot(}
all.equal(e2, 2)
\dontshow{)}

##
## 3.  name NULL in object, return default 
##
ed <- getElement2(list(a=NULL), 'a',2) # 2 with a warning
\dontshow{stopifnot(}
all.equal(ed, 2)
\dontshow{)}

e. <- getElement2(list(a=NULL), 'a', 2, warn.NULL=FALSE) # NULL
\dontshow{stopifnot(}
all.equal(e., NULL)
\dontshow{)}

eNULL <- getElement2(list(a=NULL), 'a', NULL) # NULL
\dontshow{stopifnot(}
all.equal(eNULL, NULL)
\dontshow{)}

##
## 4.  Language:  find, eval, return 
##
Qte <- quote(plot(1:4, y=x, col=c2))
if(require(pryr)){ 
  Qt <- pryr::standardise_call(Qte) # add the name 'x' 
  fn <- getElement2(Qt)
  eQuote <- getElement2(Qt, 'y')
  Col2 <- getElement2(Qt, 'col', envir=list(c2=2))
# check
\dontshow{stopifnot(}
  all.equal(fn, 'plot')
\dontshow{)}
\dontshow{stopifnot(}
  all.equal(eQuote, 1:4)
\dontshow{)}
\dontshow{stopifnot(}
  all.equal(Col2, 2)
\dontshow{)}
}
}
\keyword{manip}
