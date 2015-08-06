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
    \item{date}{date of observation (yymmdd)}
    \item{day}{day of the week (a factor)}
    \item{dm}{exchange rate Dollar/Deutsch Mark}
    \item{ddm}{dm-dm(-1)}
    \item{bp}{exchange rate of Dollar/British Pound}
    \item{cd}{exchange rate of Dollar/Canadian Dollar}
    \item{dy}{exchange rate of Dollar/Yen}
    \item{sf}{exchange rate of Dollar/Swiss Franc}
  }
}
\references{
    Verbeek, Marno (2004) \emph{A guide to modern econometrics}, John Wiley and Sons, \url{https://feb.kuleuven.be/GME/}, chapter 8.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
