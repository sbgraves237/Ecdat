\name{Tobacco}
\docType{data}
\alias{Tobacco}
\title{Households Tobacco Budget Share }
\description{
  a cross-section from 1995-96

 \emph{number of observations} :  2724

 \emph{observation} :  individuals

   \emph{country} :  Belgium

}
\usage{data(Tobacco)}
\format{A dataframe containing :
  \describe{
    \item{occupation}{
      a factor with levels (\code{bluecol}, \code{whitecol},
      \code{inactself}), the last level being inactive and 
      self-employed
    }
    \item{region}{
      a factor with levels (\code{flanders}, \code{wallon},
      \code{brussels})
    }
    \item{nkids}{number of kids of more than two years old}
    \item{nkids2}{number of kids of less than two years old}
    \item{nadults}{number of adults in household}
    \item{lnx}{log of total expenditures}
    \item{stobacco}{
      budget share of tobacco
    }
    \item{salcohol}{
      budget share of alcohol
    }
    \item{age}{age in brackets (0-4)}
  }
}
\source{
  National Institute of Statistics (NIS), Belgium.
}
\references{
    Verbeek, Marno (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, chapter 7.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
