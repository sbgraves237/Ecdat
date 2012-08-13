\name{Unemployment}
\docType{data}
\alias{Unemployment}
\title{Unemployment Duration }
\description{
  a cross-section from 1993

 \emph{number of observations} :  452

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Unemployment)}
\format{A dataframe containing :
  \describe{
    \item{duration}{duration of first spell of unemployment, t, in weeks  }
    \item{spell}{1 if spell is complete}
    \item{race}{one of nonwhite, white}
    \item{sex}{one of male, female}
    \item{reason}{reason for unemployment, one of new (new entrant), lose (job loser), leave (job leaver), reentr (labor force reentrant)}
    \item{search}{'yes' if (1) the unemployment spell is completed between the first and second surveys and number of methods used to search > average number of methods used across all records in the sample, or, (2) for individuals who remain unemployed for consecutive surveys, if the number of methods used is strictly nondecreasing at all survey points, and is strictly increasing at least at one survey point}
    \item{pubemp}{'yes' if an individual used a public employment agency to search for work at any survey points relating to the individuals first unemployment spell}
    \item{ftp1}{1  if an individual is searching for full time work at survey 1}
    \item{ftp2}{1  if an individual is searching for full time work at survey 2}
    \item{ftp3}{1  if an individual is searching for full time work at survey 3}
    \item{ftp4}{1  if an individual is searching for full time work at survey 4}
    \item{nobs}{number of observations on the first spell of unemployment for the record}
  }
}
\source{
    Romeo, Charles J. (1999) \dQuote{Conducting inference in semiparametric duration models under inequality restrictions on the shape of the hazard implied by the job search theory}, \emph{Journal of Applied Econometrics}, \bold{14(6)}, 587--605.

}
\references{
      Journal of Applied Econometrics data archive : \url{http://jae.wiley.com/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
