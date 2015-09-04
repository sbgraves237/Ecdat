\name{Dates3to1}
\alias{Dates3to1}
\title{
  Convert 3-column dates in data to class Date 
}
\description{
  Return a \code{\link{data.frame}} with columns of 
  class "Date" replaceing all 3-column dates.  
}
\usage{
Dates3to1(data, YMD=c('Year', 'Month', 'Day'))
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}} assumed to include 
    dates coded in three column sets with names 
    matching \code{YMD}.  
  }
  \item{YMD}{
    a character vector of lenght 3 of patterns to use 
    in \code{\link{grep}} to identify triples of 
    columns coding \code{YMD} in 
    \code{col.names(data)}.  
  }
}
\details{
  The data sets from the 
  \href{http://correlatesofwar.org/data-sets/COW-war}{Correlates 
  of War} project include dates coded in triples of columns with 
  names like c("StartMonth1", "StartDay1", "StartYear1", 
  "EndMonth1", ..., "EndYear2").  This function will accept a 
  \code{data.frame} obtained via \code{\link{read.csv}}
  of such a file and replace each such triple with a singe 
  column of class 'Date' combining the triple appropriately.  
}
\value{
  Return a \code{data.frame} containing the information in 
  \code{data} reformatted as described above.  
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{dateCols}}
  \code{\link{Date3to1}}
}
%\references{}
\examples{
cow0 <- data.frame(rec=1:3, startMonth=4:6, startDay=7:9, 
    startYear=1971:1973, endMonth1=10:12, endDay1=13:15, 
    endYear1=1974:1976, txt=letters[1:3])

cow0. <- Dates3to1(cow0)

# check 
cow0x <- data.frame(rec=1:3, txt=letters[1:3], 
    start=as.Date(c('1971-04-07', '1972-05-08', '1973-06-09')), 
    end1=as.Date(c('1974-10-13', '1975-11-14', '1976-12-15')) )

\dontshow{stopifnot(}
all.equal(cow0., cow0x)
\dontshow{)}
}
\keyword{manip}
