\name{Benefits}
\docType{data}
\alias{Benefits}
\title{Unemployement of Blue Collar Workers }
\description{
  a cross-section from 1972

 \emph{number of observations} :  4877

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Benefits)}
\format{A time serie containing :
  \describe{
    \item{stateur}{state unemployment rate (in \%)}
    \item{statemb}{state maximum benefit level}
    \item{state}{state of residence code}
    \item{age}{age in years}
    \item{tenure}{  years of tenure in job lost}
    \item{joblost}{a factor with levels (slack\_work,position\_abolished,seasonal\_job\_ended,other)}
    \item{nwhite}{non-white ?}
    \item{school12}{more than 12 years of school ?}
    \item{sex}{a factor with levels (male,female) }
    \item{bluecol}{blue collar worker ?}
    \item{smsa}{lives is smsa ?}
    \item{married}{married ?}
    \item{dkids}{has kids ?}
    \item{dykids}{has young kids (0-5 yrs) ?}
    \item{yrdispl}{ year of job displacement (1982=1,..., 1991=10)}
    \item{rr}{replacement rate}
    \item{head}{is  head of household ?}
    \item{ui}{ applied for (and received) UI benefits ?}
  }
}
\source{
    McCall, B.P. (1995) \dQuote{The impact of unemployment insurance benefit levels on recipiency}, \emph{Journal of Business and Economic Statistics}, \bold{13}, 189--198.

}
\references{
    Verbeek, Marno (2004) \emph{A guide to modern econometrics}, John Wiley and Sons, \url{http://www.econ.kuleuven.ac.be/GME}, chapter 7.

      Journal of Business Economics and Statistics web site : \url{http://www.amstat.org/publications/jbes/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}

\keyword{datasets}
