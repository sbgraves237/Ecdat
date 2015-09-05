\name{Date3to1}
\alias{Date3to1}
\title{
  Convert three YMD vectors to a Date 
}
\description{
  Given a \code{\link{data.frame}} with 3 columns, assume 
  they represent Year, Month and Day and return a vector 
  of class "Date".  
}
\usage{
Date3to1(data, default='Start')
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}} with 3 columns assumed to 
    represent Year, Month and Day.    
  }
  \item{default}{
    A character string to indicate how missing months and 
    days should be treated.  If the first letter is "S" 
    or "s", the default month will be 1 and the default 
    day will be 1.  Otherwise, "End" is assumed, for 
    which the default month will be 12 and the default 
    day will be the last day of the month.  
    
    NOTE:  Any number outside the range of 1 to the 
    last day of the month is considered missing and 
    its subscript is noted in the optional attribute 
    "missing".  
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
  Returns an object of class "Date" with an optional 
  attribute "missing" giving the indices of any elements 
  with missing months or days, for which a default month
  or day was supplied.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{dateCols}}
}
%\references{}
\examples{
date.frame <- data.frame(Year=c(NA, -1, 1971:1979), 
      Month=c(1:2, -1, NA, 13, 2, 12, 6:9), 
      Day=c(0, 0:6, NA, -1, 32) )
     
DateVecS <- Date3to1(date.frame)
DateVecE <- Date3to1(date.frame, "End")
                         
# check 
na <- c(1:5, 9:11)
DateVs <- as.Date(c(NA, NA, 
  '1971-01-01', '1972-01-01', '1973-01-01', 
  '1974-02-04', '1975-12-05', '1976-06-06', 
  '1977-07-01', '1978-08-01', '1979-09-01') ) 
DateVe <- as.Date(c(NA, NA, 
  '1971-12-31', '1972-12-31', '1973-12-31', 
  '1974-02-04', '1975-12-05', '1976-06-06', 
  '1977-07-31', '1978-08-31', '1979-09-30') ) 

attr(DateVs, 'missing') <- na
attr(DateVe, 'missing') <- na

\dontshow{stopifnot(}
all.equal(DateVecS, DateVs)
\dontshow{)}
\dontshow{stopifnot(}
all.equal(DateVecE, DateVe)
\dontshow{)}
}
\keyword{manip}
