\name{bankingCrises}
\docType{data}
\alias{bankingCrises}
\title{
  Countries in Banking Crises
}
\description{
  A \code{data.frame} identifying which of 70 countries had a banking
  crisis each year 1800:2010. The first column is \code{year}.  The
  remaining columns carry the names of the countries;  those columns are
  1 for years with banking crises and 0 otherwise.

}
\usage{data(bankingCrises)}
\format{ A \code{data.frame} }
\details{
  This file was created using the following command:

  bankingCrises <- readFinancialCrisisFiles(FinancialCrisisFiles)

  This is documented further in the help file for
  \code{\link{readFinancialCrisisFiles}}.

  This is an update of a subset of the data used to create Figure 10.1.
  Capital Mobility and the Incidence of Banking Crises, All Countries,
  1800-2008, Reinhart and Rogoff (2009, p. 156).
}
\source{
  \url{http://www.reinhartandrogoff.com}
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) This Time Is
  Different:  Eight Centuries of Financial Folly, Princeton U. Pr.
}
\seealso{
  \code{\link{readFinancialCrisisFiles}}
}
\examples{
data(bankingCrises)
numberOfCrises <- rowSums(bankingCrises[-1], na.rm=TRUE)
plot(bankingCrises$year, numberOfCrises, type='b')

# Write to a file for Wikimedia Commons
svg('bankingCrises.svg')
plot(bankingCrises$year, numberOfCrises, type='b', cex=0.5,
     xlab='', ylab='', bty='n')
dev.off()

}
\keyword{datasets}
