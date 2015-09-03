\name{Date3to1}
\alias{Date3to1}
\title{
  Convert three YMD vectors to a Date 
}
\description{
  Given a \code{\link{data.frame}} with 3 columns, assume 
  they represent Year, Month and Day and return a vector 
  of class "Date"
}
\usage{
Date3to1(data)
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}} with 3 columns assumed to 
    represent Year, Month and Day.    
  }
}
\details{
  The data sets from the 
  \href{http://correlatesofwar.org/data-sets/COW-war}{Correlates 
  of War} project include dates coded in triples of columns with 
  names like c("StartMonth1", "StartDay1", "StartYear1", 
  "EndMonth1", ..., "EndYear2").  This function will accept one 
  triple and translate it into a vector of class "Date". 
}
\value{
  Returns a named list of integer vectors of length 3 identifying 
  the positions in \code{col.names} of the desired date components.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{dateCols}}
}
%\references{}
\examples{
date.frame <- data.frame(Year=1971:1973, Month=1:3, Day=4:6)

DateVec <- Date3to1(date.frame)

# check 
DateV <- as.Date(c('1971-01-04', '1972-02-05', '1973-03-06'))
\dontshow{stopifnot(}
all.equal(DateVec, DateV)
\dontshow{)}
}
\keyword{manip}
