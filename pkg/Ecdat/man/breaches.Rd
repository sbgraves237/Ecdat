\name{breaches}
\docType{data}
\alias{breaches}
\title{ Cyber Security Breaches }
\description{
  \code{data.frame} of cyber security breaches involving 
  health care records of 500 or more humans reported to 
  the U.S. Department of Health and Human Services (HHS) 
  as of June 27, 2014.   
}
\usage{data(breaches)}
\format{
  A \code{data.frame} with 1055 observations on the 
  following 24 variables:
  \describe{
    \item{Number}{
      integer record number in the HHS data base 
    }
    \item{Name_of_Covered_Entity}{
      \code{\link{factor}} giving the name of the entity 
      experiencing the breach 
    }
    \item{State}{
      Factor giving the 2-letter code of the state where 
      the breach occurred.  This has 52 levels for the 50 
      states plus the District of Columbia (DC) and 
      Puerto Rico (PR).  
    }
    \item{Business_Associate_Involved}{
      Factor giving the name of a subcontractor (or blank) 
      associated with the breach.  
    }     
    \item{Individuals_Affected}{
      \code{\link{integer}} number of humans whose records 
      were compromised in the breach.  This is 500 or greater;  
      U.S. law requires reports of breaches involving 500 or 
      more records but not of breaches involving fewer.  
    }
    \item{Date_of_Breach}{
      \code{\link{character}} vector giving the date or date 
      range of the breach.  Recodes as \code{\link{Date}}s in 
      \code{breach_start} and \code{breach_end}.  
    }
    \item{Type_of_Breach}{
      \code{\link{factor}} with 29 levels giving the type of 
      breach (e.g., "Theft" vs., "Unauthorized 
      Access/Disclosure", etc.)
    }
    \item{Location_of_Breached_Information}{ 
      \code{\link{factor}} with 41 levels coding the 
      location from which the breach occurred (e.g., "Paper",
      "Laptop", etc.)
    }
    \item{Date_Posted_or_Updated}{
      \code{\link{Date}} the information was posted to the HHS 
      data base or last updated.  
    }
    \item{Summary}{
      \code{\link{character}} vector of a summary of the 
      incident.  
    }
    \item{breach_start}{
      \code{\link{Date}} of the start of the incident = first 
      date given in \code{Date_of_Breach} above.  
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
\details{
  The data primarily consists of breaches that occurred from 2010 
  through early 2014 when the extract was taken.  However, a few 
  breaches are recorded including 1 from 1997, 8 from 2002-2007, 
  13 from 2008 and 56 from 2009.  The numbers of breaches from 2010 
  - 2014 are 211, 229, 227, 254 and 56, respectively.  (A chi-square
  test for equality of the counts from 2010 through 2013 is 4.11, 
  which with 3 degrees of freedom has a significance probability of 
  0.25.  Thus, even though the lowest number is the first and the 
  largest count is the last, the apparent trend is  not 
  statistically significant under the usual assumption of 
  independent Poisson trials.)  
  
  The following corrections were made to the file:  
  
  \tabular{rlll}{
    Number \tab Name of Covered Entity \tab Corrections\cr
    \cr
    45 \tab Wyoming Department of Health \tab Cause of breach was 
    missing.  Added "Unauthorized \cr
    \tab \tab Access / Disclosure" per 
    \href{http://www.smartbrief.com/03/29/10/5-more-organizations-added-hhs-online-data-breach-list-0}{smartbreif.com/03/29/10} \cr
    
    55 \tab Reliant Rehabilitation Hospital North
    \tab Cause of breach was missing.  Added "Unauthorized \cr

    \tab Houston \tab Access / Disclosure" per Dissent. "Two Breaches 
    \cr
    \tab \tab Involving Unauthorized Access Lead to Notification." 
    \cr
    \tab \tab \href{http://www.phiprivacy.net/two-breaches-involving-unauthorized-access-lead-to-notification/}{PHIprivacy.net. 
    N.p., 20 Apr. 2010.} \cr 

    123 \tab Aetna \tab Cause of breach was missing. Added Improper \cr
    \tab \tab disposal per \href{http://www.aetna.com/news/newsReleases/2010/0630_File_Cabinet_Final.html}{
    Aetna.com/news/newsReleases/2010/0630}
    \cr 
    
    157 \tab Mayo Clinic \tab Cause of breach was missing.  Added 
    Unauthorized \cr 
    \tab \tab Access/Disclosure per Anderson, Howard. 
    "Mayo Fires \cr
    \tab \tab "Employees in 2 Incidents: Both Involved \cr 
    \tab \tab Unauthorized Access to Records." \cr
    \tab \tab \href{http://www.databreachtoday.com/mayo-fires-employees-in-2-incidents-a-2974}{Data Breach Today. N.p., 4 Oct. 2010}\cr
  
    341 \tab Saint Barnabas MedicL Center \tab Misspelled "Saint 
    Barnabas Medical Center" \cr 
  
    347 \tab Americar Health Medicare \tab Misspelled "American 
    Health Medicare" \cr
  
    484 \tab Lake Granbury Medicl Ceter \tab Misspelled "Lake 
    Granbury Medical Center" \cr 
  
    782 \tab See list of Practices under Item 9 \tab Replaced name 
    as "Cogent Healthcare, Inc." checked \cr
    \tab \tab from XML and web documents
    \cr 
  
    805 \tab Dermatology Associates of Tallahassee \tab Had 
    00/00/0000 on breach date.  This was crossed \cr
    \tab \tab check to determine that it was Sept 4, 2013 with 
    916 records \cr
  
    815 \tab Santa Clara Valley Medical Center \tab Mistype breach 
    year as 09/14/2913 corrected as 09/14/2013 \cr
  
    961 \tab Valley View Hosptial Association \tab Misspelled 
    "Valley View Hospital Association" \cr 

    1034 \tab Bio-Reference Laboratories, Inc. \tab Date changed 
    from 00/00/000 to 2/02/2014 as \cr
    \tab \tab subsequently determined.  \cr
  }
}
\source{
  U.S. Department of Health and Human Services: Health Information 
  Privacy: \href{https://ocrportal.hhs.gov/ocr/breach/breach_report.jsf}{Breaches Affecting 500 or More Individuals}
}
%\references{}
%\author{Spencer Graves}
\seealso{
  \code{\link{HHSCyberSecurityBreaches}} for a version of 
  these data downloaded more recently.  This newer version 
  includes changes in reporting and in the variables included
  in the \code{\link{data.frame}}.  
}
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
