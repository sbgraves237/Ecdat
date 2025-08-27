\name{DoctorAUS}
\docType{data}
\alias{DoctorAUS}
\title{Doctor Visits in Australia }
\description{
  a cross-section from 1977--1978

 \emph{number of observations} :  5190

 \emph{observation} :  individuals

   \emph{country} :  Australia

}
\usage{data(DoctorAUS)}
\format{A dataframe containing :
  \describe{
    \item{sex}{sex}
    \item{age}{age}
    \item{income}{
      annual income in tens of thousands of dollars
    }
    \item{insurance}{
      insurance contract (\code{medlevy} : 
      \code{medibanl} \code{levy}, \code{levyplus} : 
      private health insurance, \code{freepoor} : 
      government insurance due to low income, 
      \code{freerepa} : government insurance due to 
      old age disability or veteran status
    }
    \item{illness}{number of illness in past 2 weeks}
    \item{actdays}{
      number of days of reduced activity in past 2 
      weeks due to illness or injury
    }
    \item{hscore}{
      general health score using Goldberg's method 
      (from 0 to 12)
    }
    \item{chcond}{
      chronic condition (\code{np} : no problem, 
      \code{la} : limiting activity, \code{nla} : 
      not limiting activity)
    }
    \item{doctorco}{
      number of consultations with a doctor or 
      specialist in the past 2 weeks
    }
    \item{nondocco}{ 
      number of consultations with non-doctor 
      health professionals (chemist, optician, 
      physiotherapist, social worker, district 
      community nurse, chiropodist or 
      chiropractor) in the past 2 weeks
    }
    \item{hospadmi}{ 
      number of admissions to a hospital, 
      psychiatric hospital, nursing or 
      convalescent home in the past 12 months 
      (up to 5 or more admissions which is 
      coded as 5)
    }
    \item{hospdays}{
      number of nights in a hospital, etc. 
      during most recent admission: taken, 
      where appropriate, as the mid-point of 
      the intervals 1, 2, 3, 4, 5, 6, 7, 8-14, 
      15-30, 31-60, 61-79 with 80 or more 
      admissions coded as 80. If no admission 
      in past 12 months then equals zero.
    }
    \item{medecine}{
      total number of prescribed and 
      nonprescribed medications used in past 2 days
    }
    \item{prescrib}{ 
      total number of prescribed medications used 
      in past 2 days
    }
    \item{nonpresc}{
      total number of nonprescribed medications 
      used in past 2 days
    }
  }
}
\source{
    Cameron, A.C.  and  P.K.  Trivedi (1986) \dQuote{Econometric Models Based on Count Data: Comparisons and Applications of  Some Estimators and Tests}, \emph{Journal of Applied Econometrics}, \bold{1}, 29-54..

}
\references{
    Cameron, A.C.  and   Trivedi P.K. (1998) \emph{Regression analysis of count data}, Cambridge University Press, \url{https://cameron.econ.ucdavis.edu/racd/racddata.html}, chapter 3.

}
\seealso{
  \code{\link{Doctor}}, 
  \code{\link{DoctorContacts}}, 
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}}
}


\keyword{datasets}
