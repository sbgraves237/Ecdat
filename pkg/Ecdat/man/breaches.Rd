\name{breaches}
\docType{data}
\alias{breaches}
\title{ Cyber Security Breaches }
\description{
  \code{data.frame} of cyber security breaches involving health care 
  records of 500 or more humans reported to the U.S. Department of 
  Health and Human Services (HHS) as of June 27, 2014.   
}
\usage{data(breaches)}
\format{
  A \code{data.frame} with 1055 observations on the following 24 
  variables:
  \describe{
    \item{Number}{
      integer record number in the HHS data base 
    }
    \item{Name_of_Covered_Entity}{
      \code{\link{factor}} giving the name of the entity experiencing 
      the breach 
    }
    \item{State}{
      Factor giving the 2-letter code of the state where the breach 
      occurred.  This has 52 levels for the 50 states plus the 
      District of Columbia (DC) and Puerto Rico (PR).  
    }
    \item{Business_Associate_Involved}{
      Factor giving the name of a subcontractor (or blank) associated 
      with the breach.  
    }     
    \item{Individuals_Affected}{
      \code{\link{integer}} number of humans whose records were 
      compromised in the breach.  This is 500 or greater;  U.S. law 
      requires reports of breaches involving 500 or more records but 
      not of breaches involving fewer.  
    }
    \item{Date_of_Breach}{
      \code{\link{character}} vector giving the date or date range of 
      the breach.  Recodes as \code{\link{Date}}s in 
      \code{breach_start} and \code{breach_end}.  
    }
    \item{Type_of_Breach}{
      \code{\link{factor}} with 29 levels giving the type of breach 
      (e.g., "Theft" vs., "Unauthorized Access/Disclosure", etc.)
    }
    \item{Location_of_Breached_Information}{ 
      \code{\link{factor}} with 41 levels coding the location from 
      which the breach occurred (e.g., "Paper", "Laptop", etc.)
    }
    \item{Date_Posted_or_Updated}{
      \code{\link{Date}} the information was posted to the HHS data 
      base or last updated.  
    }
    \item{Summary}{
      \code{\link{character}} vector of a summary of the incident.  
    }
    \item{breach_start}{
      \code{\link{Date}} of the start of the incident = first date 
      given in \code{Date_of_Breach} above.  
    }
    \code{breach_end}{
      \code{\link{Date}} of the end of the incident or \code{NA} if 
      only one date is given in \code{Date_of_Breach} above.  
    }
    \code{year}{
      \code{\link{integer}} giving the year of the breach 
    }
  }
}
%\details{}
\source{
  U.S. Department of Health and Human Services: Health Information 
  Privacy: \href{http://www.hhs.gov/ocr/privacy/hipaa/administrative/breachnotificationrule/breachtool.html}{Breaches Affecting 500 or More Individuals}
}
%\references{}
%\author{Spencer Graves}
%\seealso{}
\examples{
data(breaches)
quantile(breaches$Individuals_Affected)
# confirm that the smallest number is 500 
# -- and the largest is 4.9e6
# ... and there are no NAs

dDays <- with(breaches, breach_end - breach_start)
quantile(dDays, na.rm=TRUE)
# confirm that breach_end is NA or is later than 
# breach_start 
}
\keyword{datasets}
