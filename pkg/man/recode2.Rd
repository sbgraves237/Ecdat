\name{recode2}
\alias{recode2}
\title{
  bivariate recode
}
\description{
  Recode \code{x1} and \code{x2} per the lexical \code{codes} table.
}
\usage{
recode2(x1, x2, codes)
}
\arguments{
  \item{x1, x2}{
    vectors of the same length assuming a discrete number of levels
  }
  \item{codes}{
    a 2-dimensional matrix indexed by the levels of \code{x1} and
    \code{x2}.  If \code{dimnames(codes)} are not provided, they are
    assumed to \code{unique(x1)} (or \code{unique(x2)}).
  }
}
\details{
  1.  If length(x1) != length(x2), complain.

  2.  if(is.logical(x1)) l1 <- c(FALSE, TRUE) else l1 <- unque(x1);
  ditto for x2.

  3.  If(missing(codes)) codes <- outer(unique(x1), unique(x2))

  4.  if(is.null(dim(codes))) dim(codes) <- c(length(unique(x1)),
  length(unique(x2)))

  5.  If is.null(rownames(codes)), set as follows:  If nrow(codes) ==
  length(unique(x1)), rownames(codes) <- unique(x1).  Else, if
  nrow(codes) = max(x1), set rownames(codes) <- seq(1, max(x1).  Else
  throw an error.  Ditto for colnames, ncol, and x2.

  6.  codes[x1, x2]
}
\value{
  a vector of the same length as x1 and x2.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{dim}}
  \code{\link{rownames}}
  \code{link{colnames}}
}
%\references{}
\examples{
contrib <- c(-1, 0, 0, 1)
contrib0 <- c(FALSE, FALSE, TRUE, FALSE)

contribCodes <- recode2(contrib>0, contrib0,
   c('returned', 'received', '0', 'ERR') )

cC <- c('returned', 'returned', '0', 'received')
\dontshow{stopifnot(}
all.equal(contribCodes, cC)
\dontshow{)}

}
\keyword{manip}
