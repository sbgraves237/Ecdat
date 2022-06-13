\name{CRSPmon}
\docType{data}
\alias{CRSPmon}
\title{Monthly Returns from the CRSP Database }
\description{
  monthly observations from 1969-1 to 1998-12

 \emph{number of observations} :  360

 \emph{observation} :  production units

   \emph{country} :  United States

}
\usage{data(CRSPmon)}
\format{A time series containing :
  \describe{
    \item{ge}{the return for General Electric, 
      \href{https://www.crsp.org/products/documentation/crsp-link}{PERMNO} 12060}
    \item{ibm}{ the return for IBM, PERMNO 12490}
    \item{mobil}{
      the return for Mobil Corporation, 
      PERMNO 15966
    }
    \item{crsp}{ 
      the return for the CRSP value-weighted index,
      including  dividends
    }
  }
}
\source{
    Center for Research in Security Prices, Graduate School of  Business, University of Chicago,  725 South Wells - Suite 800,  Chicago, Illinois  60607,  \url{https://www.crsp.org}.

}
\references{
    Davidson, R.  and  James G.  MacKinnon (2004) \emph{Econometric Theory and Methods}, New York, Oxford University Press, chapter 13.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
