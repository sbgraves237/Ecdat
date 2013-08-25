\name{parseCommas}
\alias{parseCommas}
\alias{parseCommas.default}
\alias{parseCommas.data.frame}
\title{
  Convert character string with Dollar signs and commas to numerics
}
\description{
  as.numeric of character strings after suppressing commas and dollar
  signs.  This is a generalization of \code{\link{parseDollars}}.
}
\usage{
parseCommas(x, pattern='\\\\$|,', replacement='',
        acceptableErrorRate=0, ...)
\method{parseCommas}{default}(x, pattern='\\\\$|,', replacement='',
        acceptableErrorRate=0, ...)
\method{parseCommas}{data.frame}(x, pattern='\\\\$|,', replacement='',
        acceptableErrorRate=0, ...)
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
  \item{acceptableErrorRate}{
    number indicating the proportion of new NAs to that can be
    introduced and still assume it's numeric
  }
  \item{\dots}{
    optional arguments to pass to \code{\link{gsub}}
  }
}
\details{
  as.numeric(gsub(x, ...))

  The \code{\link{data.frame}} method outputs another
  \code{\link{data.frame}} with character or factor columns connverted
  to numerics using \code{parseDollars} whenever that can be done
  without creating \code{NA}s.
}
\value{
  Numeric vector converted from the character strings in \code{x} or a
  \code{\link{data.frame}} with columns that are obviously numbers in
  character format converted to numerics.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{gsub}}
  \code{\link{as.numeric}}
  \code{\link{parseDollars}}
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
