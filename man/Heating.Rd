\name{Heating}
\docType{data}
\alias{Heating}
\title{Heating System Choice in California Houses }
\description{
  a cross-section

 \emph{number of observations} :  900

 \emph{observation} :  households

   \emph{country} :  California

}
\usage{data(Heating)}
\format{A dataframe containing :
  \describe{
    \item{idcase}{id}
    \item{depvar}{
      heating system, one of \code{gc} (gas 
      central), \code{gr} (gas room), \code{ec} 
      (electric central), \code{er} (electric 
      room), \code{hp} (heat pump)
    }
    \item{ic.z}{
      installation cost for heating system z 
      (defined for the 5 heating systems)
    }
    \item{oc.z}{
      annual operating cost for heating system 
      z (defined for the 5 heating systems)
    }
    \item{pb.z}{
      ratio \code{oc.z/ic.z}
    }
    \item{income}{annual income of the household}
    \item{agehed}{age of the household head}
    \item{rooms}{numbers of rooms in the house}
  }
}
\references{
      Kenneth Train's home page :  \url{https://eml.berkeley.edu/~train/}.
}
\seealso{
  \code{\link{HC}}, 
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}}}


\keyword{datasets}
