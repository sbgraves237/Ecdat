\name{FinancialCrisisFiles}
\docType{data}
\alias{FinancialCrisisFiles}
\title{ Files containing financial crisis data }
\description{
  \code{FinancialCrisisFiles} is an object of class
  \code{financialCrisisFiles} created by the \code{financialCrisisFiles}
  function in \code{Ecfun}.  It describes files containing data on
  financial crises downloadable from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.
}
\usage{
data(FinancialCrisisFiles)
}
\details{
  Reinhart and Rogoff (\url{http://www.reinhartandrogoff.com}) provide
  numerous data sets analyzed in their book, "This Time Is Different:
  Eight Centuries of Financial Folly".  Of interest here are data on
  financial crises of various types for 70 countries spanning the years
  1800 - 2010, downloadable from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.

  The function \code{financialCrisisFiles} in \code{Ecfun} produces a
  list of class \code{financialCrisisFiles} describing four different
  Excel files in very similar formats with one sheet per Country and a
  few extra descriptor sheets.  The data object
  \code{FinancialCrisisFiles} is the default output of that function.
}
\value{
  \code{FinancialCrisisFiles} is a list with components carrying the
  names of files to be read.  Each component is a list of optional
  arguments to pass to \code{do.call(read.xls, ...)} to read the sheet
  with name = name of that component.

  This corresponds to the files downloaded from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}
  in January 2013 (except for the fourth, which was not available there
  because of an error with the web site but instead was obtained
  directly from Prof. Reinhart).
}
\source{
  \url{http://www.reinhartandrogoff.com}
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) This Time Is
  Different:  Eight Centuries of Financial Folly, Princeton U. Pr.
}
\author{Spencer Graves}
\seealso{
  \code{\link[gdata]{read.xls}}
}
%\examples{}
\keyword{datasets}
