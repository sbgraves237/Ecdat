\name{read.transpose}
\alias{read.transpose}
\title{
  Read a data table in transpose form
}
\description{
  Read a text (e.g., csv) file, find rows with more than 3 \code{sep}
  characters.  Parse the initial contiguous block of those into a
  \code{matrix}.  Add \code{attributes} \code{headers}, \code{footers},
  and a \code{summary}.

  The initial application for this function is to read Table 6.16.
  Income and employment by industry in the National Income and Product
  Account tables published by the Bureau of Economic Analysis of the
  United States Department of Commerce.
}
\usage{
read.transpose(file, header=TRUE, sep=',',
               na.strings='---', ...)
}
\arguments{
  \item{file}{
    the name of a file from which the data are to be read.
  }
  \item{header}{
    Logical:  Is the second column of the identified data matrix to be
    interpreted as variable names?
  }
  \item{sep}{
    The field space separator charactor.
  }
  \item{na.strings}{
    character string(s) that translate into NA
  }
  \item{\dots}{
    optional arguments for \code{\link{strsplit}}
  }
}
\details{
  1.  txt <- readLines(file)

  2.  Split into fields.

  3.  Identifiy headers, Data, footers.

  4.  Recombine the second component of each Data row if necessary so
  all have the same number of fields.

  5.  Extract variable names

  6.  Numbers?

  7. return the transpose
}
%\source{}
\value{
  A matrix of the transpose of the rows with the max number of fields
  with attributes 'headers', 'footers', 'other', and 'summary'.  If this
  matrix can be coerced to numeric with no NAs, it will be.  Otherwise,
  it will be left as character.
}
\references{
  \href{http://www.bea.gov}{
    Table 6.16.  Income and employment by industry in the National
    Income and Product Account tables published by the Bureau of
    Economic Analysis of the United States Department of Commerce.  To
    get this table from www.bea.gov, under "U.S. Economic Accounts",
    first select "Corporate Profits" under "National".  Then next to
    "Interactive Tables", select, "National Income and Product Accounts
    Tables".  From there, select "Begin using the data...".  Under
    "Section 6 - income and employment by industry", select each of the
    tables starting "Table 6.16".  As of February 2013, there were 4
    such tables available:  Table 6.16A, 6.16B, 6.16C and 6.16D.  Each
    of the last three are available in annual and quarterly summaries.
    The \code{USFinanceIndustry} data combined the first 4 rows of the 4
    annual summary tables.
  }
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

# Use the fourth example
# to ensure the code will handle commas in a name
# and NAs
nipa6.16D <- read.transpose(demoCsv[4])
str(nipa6.16D)
}
\keyword{IO}