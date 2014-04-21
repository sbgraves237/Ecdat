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
getElement2(object, name, default, warn.NULL=TRUE)
}
\arguments{
  \item{object}{
    object from which to extract component \code{name}.
  }
  \item{name}{
    Name of the element or slot to extract
  }
  \item{default}{
    default value if \code{name} is not part of \code{object}.
  }
  \item{warn.NULL}{
    logical to decide how to treat cases where \code{object} has a
    component \code{name}:  If \code{TRUE}, return \code{default} with a
    warning.  Otherwise, return \code{NULL}.
  }
}
%\details{}
\value{
  an object of the form of \code{object[[name]]};  if \code{object} does
  not have an element or slot \code{name}, return \code{default}.
}
\author{
  Spencer Graves with help from Marc Schwartz and Hadley Wickham
}
\seealso{
  \code{\link{getElement}}
}
%\references{}
\examples{
eNA <- getElement2(list(), 'ab') # NA
e0 <- getElement2(list(), 'ab', 2) # 2
e1 <- getElement2(list(ab=1), 'ab', 2) # 1
e2 <- getElement2(list(ab=1), 'a', 2) # 2
ed <- getElement2(list(a=NULL), 'a',2) # 2 with a warning
e. <- getElement2(list(a=NULL), 'a', 2, warn.NULL=FALSE) # NULL
eNULL <- getElement2(list(a=NULL), 'a', NULL) # NULL

# check

\dontshow{stopifnot(}
all.equal(eNA, NA)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(e0, 2)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(e1, 1)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(e2, 2)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(ed, 2)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(e., NULL)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(eNULL, NULL)
\dontshow{)}
}
\keyword{manip}
