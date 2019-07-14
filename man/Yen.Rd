\name{Yen}
\docType{data}
\alias{Yen}
\title{Yen-dollar Exchange Rate }
\description{
  weekly observations from 1975 to 1989

 \emph{number of observations} :  778

 \emph{observation} :  country

   \emph{country} :  Japan

}
\usage{data(Yen)}
\format{A dataframe containing :
  \describe{
    \item{date}{the date of the observation (19850104 is January, 4, 1985)}
    \item{s}{the ask price of the dollar in units of Yen in the spot market on friday of the current week}
    \item{f}{the ask price of the dollar in units of Yen in the 30-day forward market on friday of the current week}
    \item{s30}{the bid price of the dollar in units of Yen in the spot market on the delivery date on a current forward contract}
  }
}
\source{
    Bekaert, G.  and  R.  Hodrick (1993) \dQuote{On biases in the measurement of foreign exchange risk premiums}, \emph{Journal of International Money and Finance}, \bold{12}, 115-138.

}
\references{
    Hayashi, F. (2000) \emph{Econometrics}, Princeton University Press, \url{http://fhayashi.fc2web.com/hayashi_econometrics.htm}, chapter 6, 438-443.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
