\name{readFinancialCrisisFiles}
\alias{readFinancialCrisisFiles}
\title{
  banking crisis data and function to read financial crisis files
}
\description{
  Read financial crisis data in files described by an object of 
  class \code{\link{financialCrisisFiles}}.  This is designed to 
  read Excel files describing financial crises since 1800 
  downloaded from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.

  \code{bankingCrises} is a \code{data.frame} created by
  \code{readFinancialCrisisFiles()} using 3 files downloaded 
  from \url{http://www.reinhartandrogoff.com} and 1 obtained 
  from Prof. Reinhart in January 2013.
}
\usage{
readFinancialCrisisFiles(files, crisisType=7, ...)
}
\arguments{
  \item{files}{
    an object of class \code{\link{financialCrisisFiles}}.
  }
  \item{crisisType}{
    an integer (vector) between 1 and 8 indicating the type 
    of data to be retrieved:  1=independence year (not a 
    crisis but an indicator), 2=currency, 3=inflation, 4=stock 
    market, 5=domestic soveregn debt crisis, 6=external 
    sovereign debt crisis, 7=banking, 8=tally.  ("Type" 1 = 
    year.)  These are all 0 or 1 indicating the presence of
    the event in the given year.  Type 8 = sum of types 2 
    through 7.
  }
  \item{\dots}{
    arguments to pass with file and sheet name to
    \code{\link[gdata]{read.xls}} when reading a sheet of an 
    MS Excel file.  This is assumed to be the same for all 
    sheets of all files.  If this is not the case, the 
    resulting \code{financialCrisisFiles} object will have to 
    be edited manually before using it to read the data.
  }
}
\details{
  Reinhart and Rogoff (\url{http://www.reinhartandrogoff.com})
  provide numerous data sets analyzed in their book, "This Time 
  Is Different:  Eight Centuries of Financial Folly".  Of 
  interest here are data on financial crises of various types 
  for 70 countries spanning the years 1800 - 2010, downloadable 
  from
  \url{http://www.reinhartandrogoff.com/data/browse-by-topic/topics/7/}.

  The function \code{\link{financialCrisisFiles}} produces a 
  list of class \code{financialCrisisFiles} describing different 
  Excel files in very similar formats with one sheet per Country 
  and a few extra descriptor sheets.  The data object 
  \code{FinancialCrisisFiles} is the default output of that 
  function.

  \code{readFinancialCrisisFiles} reads the sheets for the 
  individual countries.
}
\source{ \url{http://www.reinhartandrogoff.com} }
\value{
  If length(crisisType) == 1, a \code{data.frame} is returned 
  with the first column being \code{year}, and with one other 
  column containing the data for that \code{crisisType} for 
  each country.

  If length(crisisType) > 1, a list is returned containing a 
  data.frame for each country.
}
\references{
  Carmen M. Reinhart and Kenneth S. Rogoff (2009) This Time 
  Is Different:  Eight Centuries of Financial Folly, Princeton 
  U. Pr.
}
\author{Spencer Graves}
\seealso{
  \code{\link[gdata]{read.xls}}
  \code{\link{financialCrisisFiles}}
}
\examples{
##
## Recreate / update the data object BankingCrises
##
library(Ecdat)
%data(FinancialCrisisFiles)
\dontrun{
bankingCrises <- readFinancialCrisisFiles(FinancialCrisisFiles)
}

##
## Toy example using local data to check the code
## and illustrate returning all the data not just one crisisType
##
Ecdat.demoFiles <- system.file('demoFiles', package='Ecdat')
Ecdat.xls <- dir(Ecdat.demoFiles, pattern='xls$',
                 full.names=TRUE)
if(require(gdata)){
  tst <- financialCrisisFiles(Ecdat.xls)

# optional tests if not CRAN   
  if(!fda::CRAN()){
  bankingCrises.tst <- readFinancialCrisisFiles(tst)
  allCrises.tst <- readFinancialCrisisFiles(tst, 1:8)

# Manually construct tst from allCrises.tst
  tst2 <- data.frame(year=1800:1999)
  tst2$Algeria <- as.numeric(allCrises.tst$Algeria[-(1:12), 8])
  tst2$CentralAfricanRep <- as.numeric(
                    allCrises.tst$CentralAfricanRep[-(1:12), 8])
  tst2$Taiwan <- as.numeric(allCrises.tst$Taiwan[-(1:11), 8])
  tst2$UK <- as.numeric(allCrises.tst$UK[-(1:11), 8])

\dontshow{stopifnot(}
all.equal(bankingCrises.tst, tst2)
\dontshow{)}

# check
data(bankingCrises)
\dontshow{stopifnot(}
all.equal(bankingCrises.tst,
   bankingCrises[1:200, c('year', 'Algeria', 'CentralAfricanRep',
                           'Taiwan', 'UK')])
\dontshow{)}
}
}
}
\keyword{datasets}
\keyword{IO}