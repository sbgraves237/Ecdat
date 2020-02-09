\name{OFP}
\docType{data}
\alias{OFP}
\title{Visits to Physician Office }
\description{
  a cross-section

 \emph{number of observations} :  4406

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(OFP)}
\format{A dataframe containing :
  \describe{
    \item{ofp}{number of physician office visits}
    \item{ofnp}{number of nonphysician office visits}
    \item{opp}{number of physician outpatient visits}
    \item{opnp}{number of nonphysician outpatient visits}
    \item{emr}{number of emergency room visits}
    \item{hosp}{number of hospitalizations}
    \item{numchron}{number of chronic conditions}
    \item{adldiff}{the person has a condition that limits activities of daily living ?}
    \item{age}{age in years (divided by 10)}
    \item{black}{is the person African--American ?}
    \item{sex}{is the person male ?}
    \item{maried}{is the person married ?}
    \item{school}{number of years of education}
    \item{faminc}{family income in 10000\$}
    \item{employed}{is the person employed ?}
    \item{privins}{
      is the person covered by private health insurance?
    }
    \item{medicaid}{is the person covered by medicaid ?}
    \item{region}{
      the region (\code{noreast}, \code{midwest}, 
      \code{west})
    }
    \item{hlth}{self-perceived health (excellent, poor, other)}
  }
}
\source{
    Deb, P.  and  P.K.  Trivedi (1997) \dQuote{Demand for Medical Care by the Elderly: A Finite Mixture Approach}, \emph{Journal of Applied Econometrics}, \bold{12}, 313-326..

}
\references{
    Cameron, A.C.  and   Trivedi P.K. (1998) \emph{Regression analysis of count data}, Cambridge University Press, \url{http://cameron.econ.ucdavis.edu/racd/racddata.html}, chapter 6.

      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
