\name{Arrows}
\alias{Arrows}
\title{
  Draw arrows between pairs of points.
}
\description{
  Generalizes \code{graphics::arrows} to allow all arguments to be
  vectors.  (As of R 3.1.0, only the first component of the
  \code{length} argument is used by \code{graphics::arrows};  others are
  ignored without a warning.)
}
\usage{
Arrows(x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30,
       code = 2, col = par("fg"), lty = par("lty"),
       lwd = par("lwd"), warnZeroLength=FALSE, ...)
}
\arguments{
  \item{x0, y0, x1, y1, length, angle,
    code, col, lty, lwd, ...}{
    as for \code{\link{arrows}}.
  }
  \item{warnZeroLength}{
    Issue a warning for zero length arrow?  \code{\link{arrow}} does;
    skip if \code{FALSE}.
  }
}
\details{
  1.  Put all arguments in a \code{data.frame} to force them to shared
  length.

  2.  Call \code{\link{arrows}} once for each row.
}
%\value{}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{arrows}}
}
%\references{}
\examples{
plot(1:3, type='n')
Arrows(1, 1, c(1, 2, 2), c(1, 2:3), col=1:3, length=c(1, .2, .6))
}
\keyword{IO}
