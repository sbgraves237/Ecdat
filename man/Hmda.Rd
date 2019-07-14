\name{Hmda}
\docType{data}
\alias{Hmda}
\alias{Hdma}
\title{The Boston HMDA Data Set }
\description{
  a cross-section from 1997-1998

 \emph{number of observations} : 2381
 \emph{observation} : individuals
 \emph{country} : United States

In package version 0.2-9 and earlier this dataset was called Hdma.
}
\usage{data(Hmda)}
\format{A dataframe containing :
  \describe{
    \item{dir}{debt payments to total income ratio}
    \item{hir}{housing expenses to income ratio}
    \item{lvr}{ratio of size of loan to assessed value of property}
    \item{ccs}{consumer credit score from 1 to 6 (a low value being a good score) }
    \item{mcs}{mortgage credit score from 1 to 4 (a low value being a good score) }
    \item{pbcr}{public bad credit record ?}
    \item{dmi}{denied mortgage insurance ?}
    \item{self}{self employed ?}
    \item{single}{is the applicant  single ?}
    \item{uria}{1989 Massachusetts unemployment rate in the applicant's industry}
    \item{condominium}{is unit a condominium ? (was called comdominiom in version 0.2-9 and earlier versions of the package)}
    \item{black}{is the applicant black ?}
    \item{deny}{mortgage application denied ?}
  }
}
\source{
    Federal Reserve Bank of Boston.

    Munnell, Alicia H.,  Geoffrey M.B.   Tootell,  Lynne E.   Browne and  James  McEneaney (1996) \dQuote{Mortgage lending in Boston: Interpreting HMDA data}, \emph{American Economic Review}, 25-53.

}
\references{
    Stock, James H.  and  Mark W.  Watson (2003) \emph{Introduction to Econometrics}, Addison-Wesley Educational Publishers, chapter 9.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
