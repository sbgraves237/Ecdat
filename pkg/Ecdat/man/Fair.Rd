\name{Fair}
\docType{data}
\alias{Fair}
\title{Extramarital Affairs Data }
\description{
  a cross-section

 \emph{number of observations} :  601

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Fair)}
\format{A dataframe containing :
  \describe{
    \item{sex}{a factor with levels (male,female)}
    \item{age}{age}
    \item{ym}{number of years married}
    \item{child}{children ? a factor }
    \item{religious}{how religious, from 1 (anti) to 5 (very)}
    \item{education}{education}
    \item{occupation}{occupation, from 1 to 7, according to hollingshead classification (reverse numbering)}
    \item{rate}{self rating of mariage, from 1 (very unhappy) to 5 (very happy)}
    \item{nbaffairs}{number of affairs in past year}
  }
}
\source{
    Fair, R. (1977) \dQuote{A note on the computation of the tobit estimator}, \emph{Econometrica}, \bold{45}, 1723-1727.

    \url{http://fairmodel.econ.yale.edu/rayfair/pdf/1978A200.PDF}.

}
\references{
    Greene, W.H. (2003) \emph{Econometric Analysis}, Prentice Hall, \url{http://www.prenhall.com/greene/greene1.html},  Table F22.2.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
