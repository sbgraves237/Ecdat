\name{NaturalPark}
\docType{data}
\alias{NaturalPark}
\title{Willingness to Pay for the Preservation of the Alentejo Natural Park }
\description{
  a cross-section from 1987

 \emph{number of observations} :  312

 \emph{observation} :  individuals

   \emph{country} :  Portugal

}
\usage{data(NaturalPark)}
\format{A dataframe containing :
  \describe{
    \item{bid1}{initial bid, in euro}
    \item{bidh}{higher bid}
    \item{bidl}{lower bid}
    \item{answers}{
      a factor with levels (\code{nn}, 
      \code{ny}, \code{yn}, \code{yy})
    }
    \item{age}{age in 6 classes}
    \item{sex}{a factor with levels (male,female) }
    \item{income}{income in 8 classes}
  }
}
\source{
    Nunes, Paulo (2000) \emph{Contingent Valuation of the Benefits of natural areas and its warmglow component}, PhD thesis 133, FETEW, \href{https://en.wikipedia.org/wiki/KU_Leuven}{KU Leuven}.

}
\references{
    Verbeek, Marno (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, chapter 7.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
