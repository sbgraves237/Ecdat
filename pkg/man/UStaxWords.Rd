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
%\details{  }
\source{
  \url{http://taxfoundation.org/article/number-words-internal-revenue-code-and-federal-tax-regulations-1955-2005}
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
