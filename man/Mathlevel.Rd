\name{Mathlevel}
\docType{data}
\alias{Mathlevel}
\title{Level of Calculus Attained for Students Taking Advanced Micro--economics }
\description{
  a cross-section from 1983 to 1986

 \emph{number of observations} :  609

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(Mathlevel)}
\format{A dataframe containing :
  \describe{
    \item{mathlevel}{highest level of math attained , an ordered factor with levels 170, 171a, 172, 171b, 172b, 221a, 221b}
    \item{sat}{sat Math score}
    \item{language}{foreign language proficiency ?}
    \item{sex}{male, female}
    \item{major}{
      one of \code{other}, \code{eco}, \code{oss}
      (other social sciences), \code{ns} (natural 
      sciences), \code{hum} (humanities)
    }
    \item{mathcourse}{
      number of courses in advanced math (0 to 3)
    }
    \item{physiccourse}{
      number of courses in physics (0 to 2)
    }
    \item{chemistcourse}{
      number of courses in chemistry (0 to 2)
    }
  }
}
\source{
    Butler, J.S.,  T. Aldrich   Finegan and  John J.  Siegfried (1998) \dQuote{Does more calculus improve student learning in intermediate micro and macroeconomic theory ?}, \emph{Journal of Applied Econometrics}, \bold{13(2)}, April, 185--202.

}
\references{
      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
