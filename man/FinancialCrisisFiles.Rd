\name{FinancialCrisisFiles}
\docType{data}
\alias{FinancialCrisisFiles}
\title{ Files containing financial crisis data }
\description{
  \code{FinancialCrisisFiles} is an object of class
  \code{financialCrisisFiles} created by the 
  \code{financialCrisisFiles} function in 
  \code{Ecfun}.  It describes files containing data 
  on financial crises downloadable from
  \url{https://web.archive.org/web/20150419090824/http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7}.
  
NOTE:  When this dataset was created it was 
downloaded from \code{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7}.  However, it was "Not Found" in testing on 2020-02-09.  Fortunately the data are still available on the Internet Archive.  
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
  
  Version 0.2-3 of the \code{Ecfun} package 
  included a function \code{financialCrisisFiles} 
  that produced a list of class
  \code{financialCrisisFiles} describing four
  different Excel files in very similar formats 
  with one sheet per Country and a few extra
  descriptor sheets.  This data object
  \code{FinancialCrisisFiles} was produced by 
  that function.  That function required the 
  \code{gdata} package, and users of that package
  were advised to terminate use of it, because 
  it was scheduled to be removed from CRAN along 
  with all packages that used it. Since   Reinhart 
  and Rogoff seemed not to be actively maintaining
  that dataset, there seemed little need to do the 
  work required to make the 
  \code{Ecfun::financialCrisisFiles} work without 
  \code{gdata}, so it was removed from \code{Ecfun}
  version 2.0-4.  
}
\value{
  \code{FinancialCrisisFiles} is a list with
  components carrying the names of files to be read. 
  Each component is a list of optional arguments to
  pass to \code{do.call(read.xls, ...)} to read the
  sheet with name = name of that component.  (This
  \code{read.xls} was part of the \code{gdata}
  package, which may no longer be available on 
  CRAN.) 

  This corresponds to the files downloaded from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}
  in January 2013 (except for the fourth, which was
  not available there because of an error with the 
  web site but instead was obtained directly from 
  Prof. Reinhart).
}
\source{
  \url{http://www.reinhartandrogoff.com}
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) 
  This Time Is Different:  Eight Centuries of
  Financial Folly, Princeton U. Pr.
}
\author{Spencer Graves}
%\seealso{\code{\link[gdata]{read.xls}}}
%\examples{}
\keyword{datasets}
