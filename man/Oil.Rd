\name{Oil}
\docType{data}
\alias{Oil}
\title{Oil Investment }
\description{
  a cross-section from 1969 to 1992

  \emph{number of observations} :  53

  \emph{observation} :  production units

  \emph{country} :  United Kingdom

}
\usage{data(Oil)}
\format{A dataframe containing :
  \describe{
    \item{dur}{
      duration of the appraisal lag in months (time
      span between discovery of an oil field and
      beginning of development, i.e. approval of 
      annex B).
    }
    \item{size}{
      size of recoverable reserves in millions 
      of barrels
    }
    \item{waterd}{depth of the sea in metres}
    \item{gasres}{
      size of recoverable gas reserves in billions 
      of cubic feet
    }
    \item{operator}{
      equity market value (in 1991 million pounds) 
      of the company operating the oil field
    }
    \item{p}{
      real after--tax oil price measured at time 
      of annex B approval
    }
    \item{vardp}{
      volatility of the real oil price process
      measured as the squared recursive standard
      errors of the regression of 
      code{pt-pt-1} on a constant
    }
    \item{p97}{
      adaptive expectations (with parameter 
      theta=0.97) for the real after--tax oil 
      prices formed at the time of annex B approval 
    }
    \item{varp97}{
      volatility of the adaptive expectations 
      (with parameter theta=0.97) for real after 
      tax oil prices measured as the squared 
      recursive standard errors of the regression 
      of \code{pt} on \code{pte(theta)}
    }
    \item{p98}{
      adaptive expectations (with parameter 
      theta=0.98) for the real after--tax oil 
      prices formed at the time of annex B approval 
    }
    \item{varp98}{
      volatility of the adaptive expectations 
      (with parameter theta=0.98) for real after 
      tax oil prices measured as the squared 
      recursive standard errors of the regression 
      of \code{pt} on \code{pte(theta)}
    }
  }
}
\source{
  Favero, Carlo A., M. Hashem Pesaran and Sunil 
  Sharma (1994) \dQuote{A duration model of irreversible oil investment : theory and empirical evidence}, \emph{Journal of Applied Econometrics}, \bold{9(S)}, S95--S112.

}
\references{
      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
