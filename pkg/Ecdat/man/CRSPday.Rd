\name{CRSPday}
\docType{data}
\alias{CRSPday}
\title{Daily Returns from the CRSP Database }
\description{
  daily observations from 1969-1-03 to 1998-12-31

 \emph{number of observations} :  2528

 \emph{observation} :  production units

   \emph{country} :  United States

}
\usage{data(CRSPday)}
\format{A dataframe containing :
  \describe{
    \item{year}{the year}
    \item{month}{the month}
    \item{day}{the day}
    \item{ge}{the return for General Electric, Permno 12060}
    \item{ibm}{ the return for IBM, Permno 12490}
    \item{mobil}{the return for Mobil Corporation, Permno 15966}
    \item{crsp}{ the return for the CRSP value-weighted index, including  dividends}
  }
}
\source{
    Center for Research in Security Prices, Graduate School of  Business, University of Chicago,  725 South Wells - Suite 800,  Chicago, Illinois  60607,  \url{http://www.crsp.com}.

}
\references{
    Davidson, R.  and  James G.  MacKinnon (2004) \emph{Econometric Theory and Methods}, New York, Oxford University Press, \url{http://www.econ.queensu.ca/ETM/}, chapter 7, 9 and 15.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}

\keyword{datasets}
