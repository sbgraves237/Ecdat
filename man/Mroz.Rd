\name{Mroz}
\docType{data}
\alias{Mroz}
\title{Labor Supply Data }
\description{
  a cross-section

 \emph{number of observations} :  753

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Mroz)}
\format{A dataframe containing :
  \describe{
    \item{work}{
      work at home in 1975? 
      (Same carData::Mroz[['lfp']] = 
      labor force participation.)
    }
    \item{hoursw}{wife's hours of work in 1975}
    \item{child6}{
      number of children less than 6 years old in household
      (Same as carData::Mroz['k5'].)
    }
    \item{child618}{
      number of children between ages 6 and 18 in household
      (Same as carData::Mroz['k618'])
    }
    \item{agew}{wife's age}
    \item{educw}{wife's educational attainment, in years}
    \item{hearnw}{
      wife's average hourly earnings, in 1975 dollars
      }
    \item{wagew}{
      wife's wage reported at the time of the 1976 
      interview  (not= 1975 estimated wage)
    }
    \item{hoursh}{husband's hours worked in 1975}
    \item{ageh}{husband's age}
    \item{educh}{husband's educational attainment, in years}
    \item{wageh}{husband's wage, in 1975 dollars}
    \item{income}{family income, in 1975 dollars}
    \item{educwm}{
      wife's mother's educational attainment, in years
    }
    \item{educwf}{
      wife's father's educational attainment, in years
    }
    \item{unemprate}{
      unemployment rate in county of residence, in 
      percentage points
    }
    \item{city}{lives in large city (SMSA) ?}
    \item{experience}{
      actual years of wife's previous labor market 
      experience
    }
  }
}
\details{
  These data seem to have come from the same source 
  as carData::Mroz, though each data set has
  variables not in the other.  The variables that
  are shared have different names.  
  
  On 2019-11-04 Bruno Rodrigues explained that 
  Ecdat::Mroz['work'] had the two labels 
  incorrectly swapped, and 
  wooldridge::mroz['inlf'] was correct; 
  wooldridge matches carData::Mroz['lfp'].  
}
\source{
    Mroz, T. (1987) \dQuote{The sensitivity of an 
    empirical model of married women's hours of work
    to economic and statistical assumptions}, 
    \emph{Econometrica}, \bold{55}, 765-799.

    1976 Panel Study of Income Dynamics.
}
\references{
    Greene, W.H. (2003) \emph{Econometric Analysis},
    Prentice Hall, 
    \url{http://www.prenhall.com/greene/greene1.html}, 
    Table F4.1.
}
\seealso{
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}}, 
  \code{\link[carData]{Mroz}}
  \code{\link[wooldridge]{mroz}}
}
\examples{
head(Mroz)

#If 'car' and / or 'carData' is also in the path, 
# then use the following to be clear that 
# you want this version: 
head(Ecdat::Mroz)
}
\keyword{datasets}
