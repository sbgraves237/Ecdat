\name{parseDollars}
\alias{parseDollars}
\title{
  Convert character string with Dollar signs and commas to numerics
}
\description{
  as.numeric of character strings after suppressing commas and dollar
  signs.  This is a special case of \code{\link{parseCommas}}.
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
  as.numeric(gsub(x, ...)).  See also \code{\link{parseCommas}}.
}
\value{
  Numeric vector converted from \code{x}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{gsub}}
  \code{\link{as.numeric}}
  \code{\link{parseCommas}}
}
%\references{}
\examples{
##
## 1.  a character vector
##
X2 <- c('-$2,500', '$5,000.50')
x2 <- parseDollars(X2)

\dontshow{stopifnot(}
all.equal(x2, c(-2500, 5000.5))
\dontshow{)}

##
## A data.frame
##
chDF <- data.frame(let=letters[1:2], Dol=X2, dol=x2)
numDF <- parseCommas(chDF)

chkDF <- chDF
chkDF$Dol <- x2
\dontshow{stopifnot(}
all.equal(numDF, chkDF)
\dontshow{)}
}
\keyword{manip}
