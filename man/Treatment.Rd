\name{Treatment}
\docType{data}
\alias{Treatment}
\title{Evaluating Treatment Effect of Training on Earnings }
\description{
  a cross-section from 1974

 \emph{number of observations} :  2675

   \emph{country} :  United States

}
\usage{data(Treatment)}
\format{A dataframe containing :
  \describe{
    \item{treat}{treated ?}
    \item{age}{age}
    \item{educ}{education in years}
    \item{ethn}{
      a factor with levels ("\code{other}",
      "\code{black}", "\code{hispanic}")
    }
    \item{married}{married ?}
    \item{re74}{real annual earnings in 1974  (pre-treatment)}
    \item{re75}{real annual earnings in 1975  (pre-treatment)}
    \item{re78}{real annual earnings in 1978  (post-treatment)}
    \item{u74}{unemployed in 1974 ?}
    \item{u75}{unemployed in 1975 ?}
  }
}
\source{
    Lalonde, R. (1986) \dQuote{Evaluating the Econometric Evaluations of Training Programs with Experimental Data}, \emph{American Economic Review}, 604--620.

    Dehejia, R.H.  and  S.  Wahba (1999) \dQuote{Causal Effects in Nonexperimental Studies: reevaluating the Evaluation of Training Programs}, \emph{Jasa}, 1053--1062.

    Dehejia, R.H.  and  S.  Wahba (2002) \dQuote{Propensity-score Matching Methods for Nonexperimental Causal Studies}, \emph{Restat}, 151--161.

}
\references{
    Cameron, A.C.  and  P.K.  Trivedi (2005) \emph{Microeconometrics : methods and applications}, Cambridge, pp. 889--95.

}
\seealso{
  \code{\link{RetSchool}}, 
  \code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
