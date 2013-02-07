\name{financialCrisisFiles}
\alias{financialCrisisFiles}
\alias{FinancialCrisisFiles}
\title{ Files containing financial crisis data }
\description{
  \code{FinancialCrisisFiles} is an object of class
  \code{financialCrisisFiles} created by the \code{financialCrisisFiles}
  function to describe files containing data on financial crisis
  downloadable from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.
}
\usage{
%data(FinancialCrisisFiles)
financialCrisisFiles(files=c("22_data.xls", "23_data.xls",
       "Varieties_Part_III.xls", "25_data.xls"), ...)
}
\arguments{
  \item{files}{
    character vector of file names
  }
  \item{\dots}{
    arguments to pass with file and sheet name to
    \code{\link[gdata]{read.xls}} when reading a sheet of an MS Excel file.
    This is assumed to be the same for all sheets of all files.  If this
    is not the case, the resulting \code{financialCrisisFiles} object
    will have to be edited manually before using it to read the data.
  }
}
\details{
  Reinhart and Rogoff (\url{http://www.reinhartandrogoff.com}) provide
  numerous data sets analyzed in their book, "This Time Is Different:
  Eight Centuries of Financial Folly".  Of interest here are data on
  financial crises of various types for 70 countries spanning the years
  1800 - 2010, downloadable from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.

  The function \code{financialCrisisFiles} produces a list of class
  \code{financialCrisisFiles} describing four different Excel files in
  very similar formats with one sheet per Country and a few extra
  descriptor sheets.  The data object \code{FinancialCrisisFiles} is the
  default output of that function.

  It does this in several steps:

  1.  Read the first sheet of each file

  2.  Extract the names of the Countries from that first sheet.

  3.  Elimiate any blank spaces in the names to convert, e.g., "Costa
  Rica" to "CostaRica".

  4.  Find the sheets corresponding to each of the compressed names.

  5.  Construct the output list.
}
\source{
  \url{http://www.reinhartandrogoff.com}
}
\value{
  \code{FinancialCrisisFiles} is a list of class
  \code{financialCrisisFiles} with components carrying the names of
  files to be read.  Each component is list with names being the names
  of sheets in the corresponding file (i.e., Countries) where the
  components are arguments for \code{\link[gdata]{read.xls}} to read that
  sheet.
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) This Time Is
  Different:  Eight Centuries of Financial Folly, Princeton U. Pr.
}
\seealso{
  \code{\link[gdata]{read.xls}}
}
\examples{
Ecdat.demoFiles <- system.file('demoFiles', package='Ecdat')
Ecdat.xls <- dir(Ecdat.demoFiles, full.names=TRUE)
tst <- financialCrisisFiles(Ecdat.xls)

\dontrun{
# check
\dontshow{stopifnot(}
all.equal(tst, data(FinancialCrisisFiles))
\dontrun{)}
}
}
\keyword{datasets}
\keyword{IO}