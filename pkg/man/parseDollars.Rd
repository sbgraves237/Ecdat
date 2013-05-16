\name{parseDollars}
\alias{parseDollars}
\title{
  Convert character string with Dollar sign to numerics
}
\description{
  as.numeric of character strings after suppressing commas and dollar
  signs.
}
\usage{
parseDollars(x, pattern='\\\\$|,', replacement='', ...)
}
\arguments{
  \item{x}{
    vector of character strings to be converted to numerics
  }
  \item{pattern}{
    regular expression to be replaced by \code{replacement}
  }
  \item{replacement}{
    Character string to substitute for each occurrence of \code{pattern}
  }
  \item{\dots}{
    optional arguments to pass to \code{\link{gsub}}
  }
}
\details{
  as.numeric(gsub(x, ...))
}
\value{
  Numeric value converted from the character strings in \code{x}
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{gsub}}
  \code{\link{as.numeric}}
}
%\references{}
\examples{
X2 <- c('-$2,500', '$5,000.50')
x2 <- parseDollars(X2)

\dontshow{stopifnot(}
all.equal(x2, c(-2500, 5000.5))
\dontshow{)}
}
\keyword{manip}
