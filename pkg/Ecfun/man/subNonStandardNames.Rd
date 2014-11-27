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
   nonStandardNames=Ecdat::nonEnglishNames, ...)
}
\arguments{
  \item{x}{
    character vector or matrix or a \code{data.frame} of character 
    vectors in which it is desired replace \code{nonStandardNames[, 1]} 
    in \code{subNonStandardCharacters(x, ...)} with the corresponding
    element of \code{nonStandardNames[, 2]}.
  }
  \item{standardCharacters, replacement, gsubList, \dots}{
    arguments passed to \code{\link{subNonStandardCharacters}}
  }
  \item{removeSecondLine}{
    logical:  If TRUE, delete anything following "\\n" and return it as
    an attribute "secondLine".  
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
  
  NOTE:  On 13 May 2013 Jeff Newmiller at the University of California, 
  Davis, wrote, 'I think it is a fools errand to think that you can 
  automatically "normalize" arbitrary Unicode characters to an ASCII 
  form that everyone will agree on.'  (This was a reply on 
  r-help@r-project.org, subject:  "Re: [R] Matching names with non-
  English characters".)  Doubtless someone has software to do a better 
  job of this than what this function does, but I've so far been unable 
  to find it in R.  If you know of a better solution to this problem, 
  I'd be pleased to hear from you.  Spencer Graves
}
\value{
  a character vector with all \code{nonStandardCharacters} replaced first
  by \code{replacement} and then by the second column of
  \code{nonStandardNames} for any that match the first column.  If a 
  secondLine is found on any elements, it is returned as a "secondLine"
  attribute.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{sub}}
  \code{\link[Ecdat]{nonEnglishNames}}
  \code{\link{subNonStandardCharacters}}
}
%\references{}
\examples{
##
## 1.  Example 
##
tstSNSN <- c('Raul', 'Ra`l', 'Torres,Raul', 'Torres, Raul',
           "Robert C. \\\\Bobby\\\\\\\\", 'Ed  \\n --Vacancy', '', '  ')
% '\\' is converted to '\' before testing this in R CMD check            
#  confusion in character sets can create
#  names like Names[2]
##
## 2.  subNonStandardNames(vector)
##
%library(Ecdat)
SNS2 <- subNonStandardNames(tstSNSN)
SNS2

# check 
SNS2. <- c('Raul', 'Raul', tstSNSN[3:4],
            'Robert C. "Bobby"', 'Ed', '', '')
attr(SNS2., 'secondLine') <- c(rep(NA, 5), ' --Vacancy', NA, NA)
SNS2.

\dontshow{stopifnot(}
all.equal(SNS2, SNS2.)
\dontshow{)}
##
## 2.  subNonStandardNames(matrix)
##
tstmat <- parseName(tstSNSN, surnameFirst=TRUE)
submat <- subNonStandardNames(tstmat)

# check 
SNSmat <- parseName(SNS2., surnameFirst=TRUE)
\dontshow{stopifnot(}
all.equal(submat, SNSmat)
\dontshow{)}

##
## 3.  subNonStandardNames(data.frame)
##
tstdf <- as.data.frame(tstmat)
subdf <- subNonStandardNames(tstdf)

# check 
SNSdf <- as.data.frame(SNSmat)
\dontshow{stopifnot(}
all.equal(subdf, SNSdf)
\dontshow{)}
}
\keyword{manip}