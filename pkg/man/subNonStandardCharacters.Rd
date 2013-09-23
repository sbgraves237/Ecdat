\name{subNonStandardCharacters}
\alias{subNonStandardCharacters}
\title{
  sub nonstandard characters with replacement
}
\description{
  Find the first and last character not in \code{standardCharacters} and
  replace all between them with \code{replacement}.  For example, a
  string like "Ruben" where "e" carries and accent and is mangled by
  some software would become something like "Rub_n" using the default
  values for \code{standardCharacters} and \code{replacement}.
}
\usage{
subNonStandardCharacters(x,
   standardCharacters=c(letters, LETTERS, ' ','.', ',', 0:9,
      '\"', "\'", '-', '_', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern='\\\\\\\\\\\\\\\\|\\\\\\\\',
      replacement='\"')),
   ... )
}
\arguments{
  \item{x}{
    character vector in which it is desired to find the first and last
    character not in \code{standardCharacters} and replace that
    substring by \code{replacement}.
  }
  \item{standardCharacters}{
    a character vector of acceptable characters to keep.
  }
  \item{replacement}{
    a character to replace the subtring starting and ending with
    characters not in \code{standardCharacters}.
  }
  \item{gsubList}{
    list of lists of \code{pattern} and \code{replacement} arguments
    to be called in succession before looking for nonStandardCharacters
  }
  \item{\dots}{
    optional arguments passed to \code{\link{strsplit}}
  }
}
\details{
  1.  for(il in 1:length(gsubList))x <- gsub(
  gsubList[[il]][["pattern"]], gsublist[[il]][['replacement']], x)

  2.  nx <- length(x)

  3.  x. <- strsplit(x, "", ...)

  4.  for(ix in 1:nx) find the first and last \code{standardCharacters}
  in x.[ix] and substitute \code{replacement} for everything in between.
}
\value{
  a character vector with everthing between the first and last character
  not in \code{standardCharacters} replaced by \code{replacement}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{sub}}, \code{\link{strsplit}},
  \code{\link{grepNonStandardCharacters}},
  \code{\link{subNonStandardNames}}
  \code{\link[tools]{encoded_text_to_latex}}
  \code{\link{subNonStandardNames}}
}
%\references{}
\examples{
# Consider Names = Ruben, Avila and Jose, where "e" and "A" in
#    these examples carry an accent.  With the default values
#    for standardCharacters and replacement, these would become
#    Rub_en, _vila, and Jos_.
#    (The standard checks for R packages complains about
#    non-standard characters, so none are included here.)
#
Names <- c('Ra`l', 'Ra`', '`l', 'Torres, Raul',
           "Robert C. \\\\Bobby\\\\\\\\")
#  confusion in character sets can create
#  names like Names[2]
Name2 <- subNonStandardCharacters(Names)

Name2. <- c('Ra_l', 'Ra_', '_l', Names[4],
            'Robert C. "Bobby"')

\dontshow{stopifnot(}
all.equal(Name2, Name2.)
\dontshow{)}

}
\keyword{manip}
