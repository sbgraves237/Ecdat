\name{UStaxWords}
\docType{data}
\alias{UStaxWords}
\title{
  Number of Words in US Tax Law
}
\description{
  A \code{data.frame} giving the numbers of words in thousands in US tax
  law for 1995 to 2005 in 10 yaer intervals.  This includes income taxes
  and all taxes in the code itself (written by congress) and regulations
  (written by government administrators).
}
\usage{data(UStaxWords)}
\format{
  A \code{data.frame} containing:
  \describe{
    \item{year}{ tax year }
    \item{IncomeTaxCode}{ number of words in thousands in the US income
      tax }
    \item{EntireTaxCode}{
      number of words in thousands in the US income tax regulations
    }
    \item{IncomeTaxRegulations}{
      number of words in thousands in all US tax regulations
    }
    \item{IncomeTaxCodeAndRegs}{
      number of words in thousands in both the code and regulations for
      the US income tax
    }
    \item{EntireTaxCodeAndRegs}{
      number of words in thousands in US tax code and regulations
    }
  }
}
\details{
  Number of words in the US tax code and federal tax regulations,
  1955-2005 according to The Tax Foundation (taxfoundation.org).  Over
  this period, there have been roughly 3 words of regulations for each
  word in the tax code.  Most of these are income tax regulations,
  which have recently contained 4.5 words for every word in code.  The
  income tax code currently includes roughly 50 percent more words than
  other tax code, though this was reversed in 1955 and 1965.  For more
  details, see taxfoundation.org.
}
\source{
  \href{http://taxfoundation.org/article/number-words-internal-revenue-code-and-federal-tax-regulations-1955-2005}{Tax
    Foundation:  Number of Words in Internal Revenue Code and Federal
    Tax Regulations, 1955-2005}
}
%\references{}
%\seealso{}
\examples{
data(UStaxWords)
plot(EntireTaxCodeAndRegs/1000 ~ year, UStaxWords, type='b',
     ylab='Millions of words in US tax code & regs')

# Write to a file for Wikimedia Commons
svg('UStaxWords.svg')
plot(EntireTaxCodeAndRegs/1000 ~ year, UStaxWords, type='b',
     ylab='Millions of words in US tax code & regs',
     ylim=c(0, max(EntireTaxCodeAndRegs)/1000))
dev.off()
}
\keyword{datasets}
