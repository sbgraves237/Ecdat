\name{Schooling}
\docType{data}
\alias{Schooling}
\title{Wages and Schooling }
\description{
  a cross-section from 1976

 \emph{number of observations} :  3010

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Schooling)}
\format{A dataframe containing :
  \describe{
    \item{smsa66}{lived in smsa in 1966 ?}
    \item{smsa76}{lived in smsa in 1976 ?}
    \item{nearc2}{grew up near 2-yr college ?}
    \item{nearc4}{grew up near 4-yr college ?}
    \item{nearc4a}{grew up near 4-year public college ?}
    \item{nearc4b}{grew up near 4-year private college ?}
    \item{ed76}{education in 1976}
    \item{ed66}{education in 1966}
    \item{age76}{age in 1976}
    \item{daded}{dads education (imputed avg if missing)}
    \item{nodaded}{dads education imputed ?}
    \item{momed}{mothers education}
    \item{nomomed}{moms education imputed ?}
    \item{momdad14}{lived with mom and dad at age 14 ?}
    \item{sinmom14}{single mom at age 14 ?}
    \item{step14}{step parent at age 14 ?}
    \item{south66}{lived in south in 1966 ?}
    \item{south76}{lived in south in 1976 ?}
    \item{lwage76}{log wage in 1976 (outliers trimmed)}
    \item{famed}{ mom-dad education class (1-9)}
    \item{black}{black ?}
    \item{wage76}{wage in 1976 (raw, cents per hour)}
    \item{enroll76}{ enrolled in 1976 ?}
    \item{kww}{the kww score}
    \item{iqscore}{ a normed IQ score}
    \item{mar76}{married in 1976 ?}
    \item{libcrd14}{library card in home at age 14 ?}
    \item{exp76}{experience in 1976}
  }
}
\source{
    National Longitudinal Survey of Young Men (NLSYM) .

    Card, D. (1995) \emph{Using geographical variation in college proximity to estimate the return to schooling} \emph{in}  Christofides, L.N.,  E.K.   Grant and  R.  Swidinsky (1995) \emph{Aspects of labour market behaviour : essays in honour of John Vanderkamp}, University of Toronto Press, Toronto .

}
\references{
    Verbeek, Marno (2004) \emph{A guide to modern econometrics}, John Wiley and Sons, \url{http://www.econ.kuleuven.ac.be/GME}, chapter 5.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
