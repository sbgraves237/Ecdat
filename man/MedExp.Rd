\name{MedExp}
\docType{data}
\alias{MedExp}
\title{Structure of Demand for Medical Care }
\description{
  Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}

 \emph{number of observations} :  5574

}
\usage{data(MedExp)}
\format{A time series containing :
  \describe{
    \item{med}{
      annual medical expenditures in 
      constant dollars excluding dental 
      and outpatient mental 
    }
    \item{lc}{
      \code{log(coinsrate+1)}  where 
      coinsurance rate is 0 to 100
    }
    \item{idp}{individual deductible plan ?}
    \item{lpi}{
      \code{log}(annual participation 
      incentive payment) or 0 if no payment 
    }
    \item{fmde}{
      \code{log}(max(medical deductible 
      expenditure)) if IDP=1 and MDE>1 or 
      0 otherwise
    }
    \item{physlim}{physical limitation ?}
    \item{ndisease}{number of chronic diseases}
    \item{health}{self--rate health (excellent,good,fair,poor)}
    \item{linc}{log of annual family income (in \$)}
    \item{lfam}{log of family size}
    \item{educdec}{years of schooling of household head}
    \item{age}{exact age}
    \item{sex}{sex (male,female)}
    \item{child}{age less than 18 ?}
    \item{black}{is household head black ?}
  }
}
\source{
    Deb, P.  and P.K.  Trivedi (2002) \dQuote{The Structure of Demand for Medical Care: Latent Class versus Two-Part Models}, \emph{Journal of Health Economics}, \bold{21}, 601--625.

}
\references{
    Cameron, A.C.  and  P.K.  Trivedi (2005) \emph{Microeconometrics : methods and applications}, Cambridge.
}
\seealso{
  \code{\link{DoctorContacts}},
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}},
  \code{\link{Index.Time.Series}}
}
\keyword{datasets}
