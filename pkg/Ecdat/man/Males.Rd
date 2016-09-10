\name{Males}
\docType{data}
\alias{Males}
\title{Wages and Education of Young Males }
\description{
  a panel of 545 observations  from 1980  to 1987

 \emph{number of observations} :  4360

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Males)}
\format{A dataframe containing :
  \describe{
    \item{nr}{identifier}
    \item{year}{year}
    \item{school}{years of schooling}
    \item{exper}{years of experience (=age-6-school)}
    \item{union}{wage set by collective bargaining ?}
    \item{ethn}{a factor with levels (black,hisp,other)}
    \item{maried}{maried ?}
    \item{health}{health problem ?}
    \item{wage}{log of hourly wage}
    \item{industry}{a factor with 12 levels}
    \item{occupation}{a factor with 9 levels}
    \item{residence}{a factor with levels (rural area, north east, nothern central, south)}
  }
}
\source{
    National Longitudinal Survey (NLS Youth Sample).

    Vella, F.  and  M.  Verbeek (1998) \dQuote{Whose wages do unions raise ? A dynamic model of unionism and wage}, \emph{Journal of Applied Econometrics}, \bold{13}, 163--183.

}
\references{
    Verbeek, Marno (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, chapter 10.

      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
