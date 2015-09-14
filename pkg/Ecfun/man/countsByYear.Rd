\name{countsByYear}
\alias{countsByYear}
\title{
  Allocate totals by year  
}
\description{
  Allocate \code{total} to \code{countByYear}
  for a constant count per day between \code{start}
  and \code{end} for multiple events.    
}
\usage{
countsByYear(data, start="Start1", end='End1', 
             total='BatDeath', event='WarName', 
             endNA=max(data[, c(start,end)]))
}
\arguments{
  \item{data}{
    a \code{\link{data.frame}} with columns 
    \code{start}, \code{end}, and \code{total}
  }
  \item{start, end}{
    columns of \code{data} of class \code{Date}
    with \code{start} <= \code{end} during which 
    \code{total} is to be allocated 
  }
  \item{total}{
    A quantity to be allocated by year giving 
    a constant rate per day.  
  }
  \item{event}{
    name of the event whose total is to be allocated.  
  }
  \item{endNA}{
    Date to use if \code{is.na(data[, end])}.  
  }
}
%\details{}
\value{
  a numeric \code{\link{matrix}} whose 
  \code{\link{colSums}} match \code{total} with names 
  for all the years between \code{start} and \code{end}.  
  The number of columns of the output matrix match the 
  number of rows of \code{data}.  The
  \code{\link{colSums}} match \code{total}.   
}
\author{
  Spencer Graves 
}
%\seealso{}
%\references{}
\examples{
##
## 1.  data.frame(WarName, Start1, End1, BatDeath)
##
start73 <- as.Date('1973-01-22')
tstWars <- data.frame(WarName=c('short', '2yr', '10yr'), 
    Start1=c(start73, start73+365, start73-365), 
    End1=start73+c(99, 2*365, NA), 
    BatDeath=c(100, 123, 456)) 
##
## 2.  do
##
deathsByYr <- countsByYear(tstWars, 
              endNA=start73+9*365.2)

# check 
Counts <- matrix(0, 11, 3, 
    dimnames=list(c(1972:1982), tstWars$WarName) )
Counts['1973', 1] <- 100
Counts[as.character(1974:1975), 2] <- with(tstWars, 
    countByYear(Start1[2], End1[2], BatDeath[2]) )
Counts[as.character(1972:1982), 3] <- with(tstWars, 
    countByYear(Start1[3], start73+9*365.2, BatDeath[3]) )

\dontshow{stopifnot(}
all.equal(deathsByYr, Counts)
\dontshow{)}
  
}
\keyword{manip}