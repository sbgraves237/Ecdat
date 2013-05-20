\name{subNonEnglishNames}
\alias{subNonEnglishNames}
\title{
  sub for nonstandard characters
}
\description{
  sub(nonEnglishData["nonEnglish"], nonEnglishData["English"], x)
}
\usage{
subNonEnglishNames(x, nonEnglishData=nonEnglishNames, ...)
}
\arguments{
  \item{x}{
    character vector in which it is desired to match each \code{pattern}
    = \code{nonEnglishData[["nonEnglish"]]} and replace with the
    corresponding \code{resplacement} =
    \code{nonEnglishData[["English"]]}.
  }
  \item{nonEnglishData}{
    a \code{\link{data.frame}} with columns \code{nonEnglish} and
    \code{English}
  }
  \item{\dots}{
    optional arguments for \code{\link{sub}}
  }
}
\details{
  loop over all rows of \code{nonEnglishData} substituting anything
  matching \code{nonEnglishData[i, 'nonEnglish']} with
  \code{nonEnglishData[i, 'English']}.
}
\value{
  a character vector with all substrings matching
  \code{nonEnglishData[['nonEnglish']]} replaced by
  \code{nonEnglishData[['English']]}
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{sub}}
  \code{\link{nonEnglishNames}}
}
%\references{}
\examples{
Names <- c('Raul', 'Ra`l', 'Torres,Raul', 'Torres, Raul')
#  confusion in character sets can create
#  names like Names[2]

data(nonEnglishNames)
Name2 <- subNonEnglishNames(Names)

Name2. <- c('Raul', 'Raul', Names[3:4])

\dontshow{stopifnot(}
#all.equal(Name2, Name2.)
\dontshow{)}

}
\keyword{manip}
