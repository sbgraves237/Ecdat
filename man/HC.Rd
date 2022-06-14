\name{HC}
\docType{data}
\alias{HC}
\title{Heating and Cooling System Choice in Newly Built Houses in California }
\description{
  a cross-section

 \emph{number of observations} :  250

 \emph{observation} :  households

   \emph{country} :  California

}
\usage{data(HC)}
\format{A dataframe containing :
  \describe{
    \item{depvar}{
      heating system, one of \code{gcc} 
      (gas central heat with cooling), 
      \code{ecc} (electric central 
      resistance heat with cooling), 
      \code{erc} (electric room 
      resistance heat with cooling), 
      \code{hpc} (electric heat pump 
      which provides cooling also), 
      \code{gc} (gas central heat without 
      cooling, \code{ec} (electric 
      central resistance heat without 
      cooling), \code{er} (electric room 
      resistance heat without cooling)
    }
    \item{ich.z}{
      installation cost of the heating 
      portion of the system
    }
    \item{icca}{installation cost for cooling}
    \item{och.z}{operating cost for the heating portion of the system}
    \item{occa}{operating cost for cooling}
    \item{income}{annual income of the household}
  }
}
\references{
      Kenneth Train's home page :  \url{https://eml.berkeley.edu/~train/}.
}
\seealso{
  \code{\link{Heating}}, 
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}}}

\keyword{datasets}
