\name{subNonStandardNames}
\alias{subNonStandardNames}
\title{
  sub for nonstandard names
}
\description{
  sub(nonEnglishData["nonEnglish"], nonEnglishData["English"], x)
}
\usage{
subNonStandardNames(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern='\\\\\\\\\\\\\\\\|\\\\\\\\',
      replacement='\"')),
   removeSecondLine=TRUE,
   nonStandardNames=nonEnglishNames, ...)
}
\arguments{
  \item{x}{
    character vector in which it is desired replace
    \code{nonStandardNames[, 1]} in
    \code{subNonStandardCharacters(x, ...)} with the corresponding
    element of \code{nonStandardNames[, 2]}.
  }
  \item{standardCharacters, replacement, gsubList, \dots}{
    arguments passed to \code{\link{subNonStandardCharacters}}
  }
  \item{removeSecondLine}{
    logical:  If TRUE, delete anything following "\\n" and return it as
    an attribute "secondLine"
  }
  \item{nonStandardNames}{
    data.frame or character matrix with two columns:  Replace any
    substring of \code{x} matching \code{nonStandardNames[, 1]} with the
    corresponding elemeng of \code{nonStandardNames[, 2]}
  }
}
\details{
  1.  removeSecondLine

  2.  x. <- subNonStandardCharacters(x, standardCharacters, replacement,
  ...)

  3.  Loop over all rows of \code{nonStandardNames} substituting anything
  matching \code{nonEnglishData[i, 1]} with \code{nonEnglishData[i,
    2]}.

  4.  Eliminate leading and trailing blanks.
}
\value{
  a character vector with all \code{nonStandardCharacters} replaced first
  by \code{replacement} and then by the second column of
  \code{nonStandardNames} for any that match the first column.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{sub}}
  \code{\link{nonEnglishNames}}
  \code{\link{subNonStandardCharacters}}
}
%\references{}
\examples{
Names <- c('Raul', 'Ra`l', 'Torres,Raul', 'Torres, Raul',
           "Robert C. \\\\Bobby\\\\\\\\", 'Ed  \n --Vacancy')
#  confusion in character sets can create
#  names like Names[2]

data(nonEnglishNames)
Name2 <- subNonStandardNames(Names)
Name2

Name2. <- c('Raul', 'Raul', Names[3:4],
            'Robert C. "Bobby"', 'Ed')
attr(Name2., 'secondLine') <- c(rep(NA, 5), ' --Vacancy')
Name2.

\dontshow{stopifnot(}
all.equal(Name2, Name2.)
\dontshow{)}

}
\keyword{manip}
