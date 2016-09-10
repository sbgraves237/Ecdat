\name{Bids}
\docType{data}
\alias{Bids}
\title{Bids Received By U.S. Firms }
\description{
  a cross-section

 \emph{number of observations} :  126

 \emph{observation} :  production units

   \emph{country} :  United States

}
\usage{data(Bids)}
\format{A dataframe containing :
  \describe{
    \item{docno}{ doc no.}
    \item{weeks}{weeks}
    \item{numbids}{ count }
    \item{takeover}{ delta (1 if taken over)}
    \item{bidprem}{ bid Premium}
    \item{insthold}{ institutional holdings}
    \item{size}{ size measured in billions}
    \item{leglrest}{  legal restructuring}
    \item{rearest}{real restructuring}
    \item{finrest}{  financial restructuring}
    \item{regulatn}{regulation}
    \item{whtknght}{white knight}
  }
}
\source{
    Jaggia, Sanjiv  and  Satish  Thosar (1993) \dQuote{Multiple Bids as a Consequence of Target Management Resistance}, \emph{Review of Quantitative Finance and Accounting}, 447--457.

    Cameron, A.C.  and  Per  Johansson (1997) \dQuote{Count Data Regression Models using Series Expansions: with Applications}, \emph{Journal of Applied Econometrics}, \bold{12}, may, 203--223.

}
\references{
    Cameron, A.C.  and   Trivedi P.K. (1998) \emph{Regression analysis of count data}, Cambridge University Press, \url{http://cameron.econ.ucdavis.edu/racd/racddata.html}, chapter 5.

      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}

\keyword{datasets}
