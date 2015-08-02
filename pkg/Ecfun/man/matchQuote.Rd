\name{matchQuote}
\alias{matchQuote}
\title{
  Match isolated quotes across records 
}
\description{
  Look for unmatched quotes in a character vector.  If found, 
  look for a matching quote starting the next characer string
  in the vector, possibly after a blank line.  If found, 
  merge the two strings and return the resulting shortened 
  character vector.  
}
\usage{
matchQuote(x,  Quote='"', sep=' ', maxChars2append=2, ...) 
}
\arguments{
  \item{x}{
    a character vector to scan for unmatched \code{Quote}s.  
  }
  \item{Quote}{
    the \code{Quote} character that should appear in pairs 
  }
  \item{sep}{
    \code{sep} argument passed to \code{\link{paste}} to 
    combine pairs of successive lines with unmatched quotes.  
  }
  \item{maxChars2append}{
    maximum number of characters in the following string to 
    concatonate two adjacent strings (possibly separated by 
    a blank line) with unmatched \code{Quote}s.   
  }
  \item{\dots}{
    optional arguments for \code{\link{gsub}} 
  }
}
\details{
  This function was written to help parse data from the US Department 
  of Health and Human Services on 
  \href{https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf}{
  cyber-security breaches affecting 500 or more individuals}.  
  As of 2014-06-03 the csv version of these data included commas 
  in quotes that are not \code{sep} characters, quotes that are 
  not matched, lines with zero characters, followed by lines 
  with 3 characters being a quote and a comma.  This function 
  was written to drop the blank lines and append the quote-comma 
  line to the preceeding line so it contained matching quotes.  
}
%\source{ }
\value{
  The input character vector possibly shortened with 
  the following attributes explaining what was found:  
  
  \itemize{
    \item{unmatchedQuotes}{
      indices of the input \code{x} with an unmatched 
      \code{Quote}.  
    }
    \item{blankLinesDropped}{
      indices of the input \code{x} that were dropped 
      because they (1) followed an unmatched \code{Quote} 
      and (2) contained no non-blank characters.  
    }
    \code{quoteLinesAppended}{
      indices of the input \code{x} that were concatonated 
      with a preceeding line because the two lines contained
      unmatched \code{Quote} characters, and concatonating 
      them produced a line with all \code{Quote}s matched.  
    }
    \code{ncharsAppended}{
      an integer vector of the same length as 
      \code{quoteLinesConcatonated} giving the number of 
      characters in the second line concatonated onto 
      the previous line.  
    }  
  } 
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{strsplit1}}
  \code{\link[tools]{delimMatch}}
}
\examples{
chvec <- c('abc', 'de"f', ' ', '",', 'g"h', 'matched"quotes"', '')
ch. <- matchQuote(chvec)

# check 
chv. <- c('abc', 'de"f ",', 'g"h', 'matched"quotes"', '')
attr(chv., 'unmatchedQuotes') <- c(2, 4, 5)
attr(chv., 'blankLinesDropped') <- 3
attr(chv., 'quoteLinesAppended') <- 4
attr(chv., 'ncharsAppended') <- 2 
\dontshow{stopifnot(}
all.equal(ch., chv.)
\dontshow{)}
}
\keyword{manip}