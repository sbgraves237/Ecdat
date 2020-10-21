\name{Garch}
\docType{data}
\alias{Garch}
\title{Daily Observations on Exchange Rates of the US Dollar Against Other Currencies }
\description{
  daily observations from 1980--01 to 1987--05--21

 \emph{number of observations} :  1867

 \emph{observation} :  country

   \emph{country} :  World

}
\usage{data(Garch)}
\format{A dataframe containing :
  \describe{
    \item{\code{date}}{
      date of observation (\code{yymmdd})
    }
    \item{\code{day}}{day of the week (a factor)}
    \item{\code{dm}}{
      exchange rate Dollar/Deutsch Mark
    }
    \item{\code{ddm}}{\code{dm-dm(-1)}}
    \item{\code{bp}}{
      exchange rate of Dollar/British Pound
    }
    \item{\code{cd}}{
      exchange rate of Dollar/Canadian Dollar
    }
    \item{\code{dy}}{
      exchange rate of Dollar/Yen
    }
    \item{\code{sf}}{
      exchange rate of Dollar/Swiss Franc
    }
  }
}
\references{
    Verbeek, Marno (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, chapter 8.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
