\name{subNonStandardCharacters}
\alias{subNonStandardCharacters}
\title{
  sub nonstandard characters with replacement
}
\description{
  First convert to ASCII, stripping standard 
  accents and special characters.  Then find 
  the first and last character not in 
  \code{standardCharacters} and replace all 
  between them with \code{replacement}.  For 
  example, a string like "Ruben" where "e" 
  carries an accent and is mangled by some 
  software would become something like 
  "Rub_n" using the default values for 
  \code{standardCharacters} and 
  \code{replacement}.
}
\usage{
subNonStandardCharacters(x,
   standardCharacters=c(letters, LETTERS, 
      ' ','.', '?', '!', ',', 0:9, '/', '*', 
      '$', '\%', '\"', "\'", '-', '+', '&', 
      '_', ';', '(', ')', '[', ']', '\n'),
   replacement='_',
   gsubList=list(list(pattern = 
      '\\\\\\\\\\\\\\\\|\\\\\\\\',
      replacement='\"')), ... )
}
\arguments{
  \item{x}{
    character vector in which it is desired 
    to find the first and last character not 
    in \code{standardCharacters} and replace 
    that substring by \code{replacement}.
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
    to be called in succession before looking for 
    nonStandardCharacters
  }
  \item{\dots}{
    optional arguments passed to \code{\link{strsplit}}
  }
}
\details{
  1.  for(il in 1:length(gsubList))x <- gsub(
  gsubList[[il]][["pattern"]], gsublist[[il]][['replacement']], x)
  
  2.  x <- stringi::stri_trans_general(x, "Latin-ASCII")

  3.  nx <- length(x)

  4.  x. <- strsplit(x, "", ...)

  5.  for(ix in 1:nx) find the first and last 
  \code{standardCharacters}
  in x.[ix] and substitute \code{replacement} for everything in
  between.
  
    
  NOTES:  
  
  ** To find the elements of x that have changed, use either 
  \code{subNonStandardCharacters(x) != x} or 
  \code{grep(replacement, subNonStandardCharacters(x))}, where 
  \code{replacement} is the \code{replacement} argument = "_" by 
  default.  
  
  ** On 13 May 2013 Jeff Newmiller at the University of California, 
  Davis, wrote, 'I think it is a fools errand to think that you can 
  automatically "normalize" arbitrary Unicode characters to an ASCII 
  form that everyone will agree on.'  (This was a reply on 
  r-help@r-project.org, subject:  "Re: [R] Matching names with non-
  English characters".)  
  
  ** On 2014-12-15 Ista Zahn suggested 
  \code{\link[stringi]{stri_trans_general}}.  (This was a reply on 
  r-help@r-project.org, subject:  "[R] Comparing Latin characters
  with and without accents?".)  
}
\value{
  a character vector with everthing between the first and last
  character not in \code{standardCharacters} replaced by 
  \code{replacement}.  
}
\author{
  Spencer Graves with thanks to Jeff Newmiller, who described this 
  as a "fool's errand", Milan Bouchet-Valat, who directed me to 
  \code{\link{iconv}}, and Ista Zahn, who suggested 
  \code{\link[stringi]{stri_trans_general}}.  
}
\seealso{
  \code{\link{sub}}, \code{\link{strsplit}},
  \code{\link{grepNonStandardCharacters}},
  \code{\link{subNonStandardNames}}
  \code{\link{subNonStandardNames}}
  \code{\link{iconv}} in the \code{base} 
      package does some conversion, but is not
      consistent across platforms, at least 
      using R 3.1.2 on 2015-01.25.  
  \code{\link[stringi]{stri_trans_general}} 
      seems better.  
}
%\references{}
\examples{
##
## 1. Consider Names = Ruben, Avila and Jose, where "e" and "A" in
##    these examples carry an accent.  With the default values
##    for standardCharacters and replacement, these might be 
##    converted to something like Rub_n, _vila, and Jos_, with 
##    different software possibly mangling the names differently.  
##    (The standard checks for R packages in an English locale 
##    complains about non-ASCII characters, because they are 
##    not portable.)
##
nonstdNames <- c('Ra`l', 'Ra`', '`l', 'Torres, Raul',
           "Robert C. \\\\Bobby\\\\\\\\", NA, '', '  ', 
           '$12', '12\%')
%NOTES: 
%   (1) "\\" gets converted to "\" before testing this.             
%   (2) "%" indicates a comment;  should test this here, 
%       but I don't see how.  
#  confusion in character sets can create
#  names like Names[2]
Name2 <- subNonStandardCharacters(nonstdNames)
str(Name2)

# check 
Name2. <- c('Ra_l', 'Ra_', '_l', nonstdNames[4],
            'Robert C. "Bobby"', NA, '', '  ', 
            '$12', '12\%')
str(Name2.)
\dontshow{stopifnot(}
all.equal(Name2, Name2.)
\dontshow{)}
##
## 2.  Example from iconv
##
icx <- c("Ekstr\\xf8m", "J\\xf6reskog", 
         "bi\\xdfchen Z\\xfcrcher")
icx2 <- subNonStandardCharacters(icx)

# check 
icx. <- c('Ekstrom', 'Joreskog', 'bisschen Zurcher')
\dontshow{stopifnot(}
all.equal(icx2, icx.)
\dontshow{)}

}
\keyword{manip}
