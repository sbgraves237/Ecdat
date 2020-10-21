\name{Car}
\docType{data}
\alias{Car}
\title{Stated Preferences for Car Choice }
\description{
  a cross-section

  \emph{number of observations} :  4654

  \emph{observation} :  individuals

  \emph{country} :  United States

}
\usage{data(Car)}
\format{A dataframe containing :
  \describe{
    \item{choice}{
      choice of a vehicle among 6 propositions
    }
    \item{college}{college education ?}
    \item{hsg2}{size of household greater than 2 ?}
    \item{coml5}{commute lower than 5 miles a day ?}
    \item{typez}{
      body type, one of \code{regcar} (regular car), 
      \code{sportuv} (sport utility vehicle), 
      \code{sportcar}, \code{stwagon} (station 
      wagon), \code{truck}, \code{van}, for each 
      proposition z from 1 to 6
    }
    \item{fuelz}{
      fuel for proposition z, one of \code{gasoline}, 
      \code{methanol}, \code{cng} (compressed natural 
      gas), \code{electric}.
    }
    \item{pricez}{
      price of vehicle divided by the logarithm of income
    }
    \item{rangez}{
      hundreds of miles vehicle  can travel between 
      refuelings/rechargings
    }
    \item{accz}{
      acceleration, tens of seconds required to 
      reach 30 mph from stop
    }
    \item{speedz}{highest attainable speed in hundreds of mph}
    \item{pollutionz}{tailpipe emissions as fraction of those for new gas vehicle}
    \item{sizez}{0 for a mini, 1 for a subcompact, 2 for a compact and 3 for a mid--size or large vehicle}
    \item{spacez}{fraction of luggage space in comparable new gas vehicle}
    \item{costz}{cost per mile of travel (tens of cents) : home recharging for electric vehicle, station refueling otherwise}
    \item{stationz}{fraction of stations that can refuel/recharge vehicle}
  }
}
\source{
    McFadden, Daniel  and  Kenneth  Train (2000) \dQuote{Mixed MNL models for discrete response}, \emph{Journal of Applied Econometrics}, \bold{15(5)}, 447--470.

}
\references{
      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}

\keyword{datasets}
