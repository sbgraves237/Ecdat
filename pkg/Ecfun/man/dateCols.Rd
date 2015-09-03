\name{dateCols}
\alias{dateCols}
\title{
  Identify YMD names in a character vector
}
\description{
  \code{\link{grep}} for YMD (year, month, day) in 
  \code{col.names}.  Return a named list of integer 
  vectors of length 3 for each triple found.  
}
\usage{
dateCols(col.names, YMD=c('Year', 'Month', 'Day'))
}
\arguments{
  \item{col.names}{
    either a character vector in which to search for names matching 
    \code{YMD} or an object with non-null \code{colnames} 
  }
  \item{YMD}{
    a character vector of patterns to use in \code{\link{grep}} to 
    identify triples of columns coding \code{YMD} in 
    \code{col.names}
  }
}
\details{
  The data sets from the 
  \href{http://correlatesofwar.org/data-sets/COW-war}{Correlates 
  of War} project include dates coded in triples of columns with 
  names like c("StartMonth1", "StartDay1", "StartYear1", "EndMonth1", 
  ..., "EndYear2").  This function will find all relevent date 
  triples in a character vector of column names and return a list 
  of integer vectors of length 3 with names like "Start1", "End1", 
  ..., "End2" giving the positions in \code{col.names} of the 
  desired date components.  
  
  Algorithm:  
  
  1.  if(!is.null(colnames(YMD)))YMD <- colnames(YMD)
  
  2.  \code{ymd} <- \code{\link{grep}} for YMD (Year, Month, Day) 
  in \code{col.names}.
  
  3.  \code{groupNames} <- \code{\link{sub}} \code{pattern} with 
  '' in \code{ymd}
  
  4.  Throw a \code{\link{warning}} for any \code{groupNames} 
  character string that does not appear with all three of Year, 
  Month, and Day.  
  
  5.  Return a list of integer vectors of length 3 for each 
  triple found.  
}
\value{
  Returns a named list of integer vectors of length 3 identifying 
  the positions in \code{col.names} of the desired date components.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{Date3to1}}
}
%\references{}
\examples{
##
## 1.  character vector 
##
colNames <- c('war', 'StartMonth1', 'StartDay1', 'StartYear1', 
  'EndMonth1', 'EndMonth2', 'EndDay2', 'EndYear2', 'Initiator')

colNums <- dateCols(colNames)
# Should issue a warning:  
# Warning message:
# In dateCols(colNames) :
#   number of matches for Year = 2 != number of matches for Month = 3

# check 
colN <- list(Start1=c(Year=4, Month=2, Day=3), 
             End2=c(Year=8, Month=6, Day=7) )
\dontshow{stopifnot(}
all.equal(colNums, colN)
\dontshow{)}
##
## 2.  array 
##
A <- matrix(ncol=length(colNames), 
      dimnames=list(NULL, colNames))

Anums <- dateCols(A)

# check 
\dontshow{stopifnot(}
all.equal(Anums, colN)
\dontshow{)}
}
\keyword{manip}
