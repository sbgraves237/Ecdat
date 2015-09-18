\name{countByYear}
\alias{countByYear}
\title{
  Allocate a total by year  
}
\description{
  Allocate \code{total} to \code{countByYear}
  for a constant count per day between \code{start}
  and \code{end}.  
}
\usage{
countByYear(start, end, total=1)
}
\arguments{
  \item{start, end}{
    objects of class "Date" specifying the start, end, 
    respectively, of the event 
  }
  \item{total}{
    A number to be allocated by year in proportion to 
    the number of days in the event each year.  
  }
}
%\details{}
\value{
  a numeric vector whose \code{\link{sum}} is 
  \code{total} with names for all the years between 
  \code{start} and \code{end} 
}
\author{
  Spencer Graves 
}
%\seealso{}
%\references{}
\examples{
##
## 1.  All in one year
##
start73 <- as.Date('1973-01-22')
tst1 <- countByYear(start73, start73+99, 123)

# check 
tst1. <- 123
names(tst1.) <- 1973
\dontshow{stopifnot(}
all.equal(tst1, tst1.)
\dontshow{)}

##
## 2.  Two years 
##
tst2 <- countByYear(start73, start73+365, 123) 

# check 
dur <- 366
days1 <- (365-21)
days2 <- 22
tst2. <- 123 * c(days1, days2)/dur 
names(tst2.) <- 1973:1974 
\dontshow{stopifnot(}
all.equal(tst2, tst2.)
\dontshow{)}

##
## 3.  Ten years 
## 
tst10 <- countByYear(start73, start73+10*365.2, 123)

# check 
days <- (c(rep(c(rep(365, 3), 366), length=10), 0)
         + c(-21, rep(0, 9), 22) )
tst10. <- 123 * days/(10*365.2+1) 
names(tst10.) <- 1973:1983 

\dontshow{stopifnot(}
all.equal(tst10, tst10.)
\dontshow{)}
          
}
\keyword{manip}