\name{readNIPA}
\alias{readNIPA}
\title{
  Read a National Income and Product Accounts data table
}
\description{
  Read multiple files with data in rows using
  \code{\link{read.transpose}} and combine the initial columns.
}
\usage{
readNIPA(files, sep.footnote='/', ...)
}
\arguments{
  \item{files}{
    A character vector of names of files from which the data are to be
    read using \code{\link{read.transpose}}.
  }
  \item{sep.footnote}{
    a single character to identify footnote references in the variable
    names in some but not all of \code{files}.
  }
  \item{\dots}{
    optional arguments for \code{\link{read.transpose}}
  }
}
\details{
  This is written first and foremost to facilitate updating
  \code{\link[Ecdat]{USFinanceIndustry}} from Table 6.16:  Income and
  employment by industry in the National Income and Product Account
  tables published by the Bureau of Economic Analysis of the United
  States Department of Commerce.  As of February 2013, this table can be
  obtained from \url{http://www.bea.gov}:  Under "U.S. Economic
  Accounts", first select "Corporate Profits" under "National".  Then
  next to "Interactive Tables", select, "National Income and Product
  Accounts Tables".  From there, select "Begin using the data...".
  Under "Section 6 - income and employment by industry", select each of
  the tables starting "Table 6.16".  As of February 2013, there were 4
  such tables available:  Table 6.16A, 6.16B, 6.16C and 6.16D.  Each of
  the last three are available in annual and quarterly summaries.  The
  \code{\link[Ecdat]{USFinanceIndustry}} data combined the first 4 rows of
  the 4 annual summary tables.

  This is available in 4 separate files, which must be downloaded and
  combined using \code{readNIPA}.  The first three of these are
  historical data and are rarely revised.  For convenience and for
  testing, they are provided in the \code{demoFiles} subdirectory of
  this \code{Ecdat} package.

  It has not been tested on other data but should work for annual data
  with a sufficiently similar structure.

  The algorithm proceeds as follows:

  1.  Data <- lapply(files, read.transpose)

  2.  Is \code{Data} a list of numeric matrices?  If no, print an
  error.

  3.  cbind common initial variables, averaging overlapping years,
  reporting percent difference

  4.  attributes:  stats from files and overlap.  Stats include the
  first and last year and the last revision date for each file, plus the
  number of years overlap with the previous file and the relative change
  in the common files kept between those two files.
}
%\source{}
\value{
  a \code{\link{matrix}} of the common variables
}
\references{
  \href{http://www.bea.gov}{United States Department of Commerce Bureau
    of Economic Analysis National Income and Product Account tables}
}
\author{Spencer Graves}
\seealso{
  \code{\link{read.table}}
  \code{\link{readLines}}
  \code{\link{strsplit}}
}
\examples{
#  Find demoFiles/*.csv
demoDir <- system.file('demoFiles', package='Ecdat')
(demoCsv <- dir(demoDir, pattern='csv$', full.names=TRUE))

nipa6.16 <- readNIPA(demoCsv)
str(nipa6.16)
}
\keyword{IO}