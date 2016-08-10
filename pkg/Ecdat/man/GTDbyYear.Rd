\name{GTDbyYear}
\docType{data}
\alias{GTDbyYear}
\title{
  Global Terrorism Database yearly summaries}
\description{
  The \href{https://en.wikipedia.org/wiki/Global_Terrorism_Database}{Global Terrorism Database (GTD)} 
  "is a database of incidents of terrorism 
  from 1970 onward".  Through 2014, this 
  database contains information on 141,966 
  incidents.  
  
  \code{GTDbyYear} provides a few summary
  statistics along with a \code{methodology} 
  \code{\link{factor}}, which \href{https://www.washingtonpost.com/news/monkey-cage/wp/2014/08/11/how-to-fix-the-flaws-in-the-global-terrorism-database-and-why-it-matters/}{Pape et al.} 
  insisted is necessary, because an increase 
  of over 70 percent in suicide terrorism 
  between 2007 and 2013 is best explained by 
  a methodology change in GTD that occurred 
  on 2011-11-01;  Pape's own 
\href{https://en.wikipedia.org/wiki/Suicide_Attack_Database}{Suicide Attack Database} 
  showed a 19 percent \emph{decrease} over
  the same period.  
}
\usage{data(GTDbyYear)}
\format{A dataframe containing :
  \describe{
    \item{year}{integer year, 1970:2014.}
    \item{methodology}{
      Dominant methodology / organization
      primarily responsible for the data
      collection in the given year.  The 
      Pinkerton Global Intelligence Service
      (PGIS) managed data collection from 
      1970-01-01 to 1997-12-31.  The Center
      for Terrorism and Intelligence Studies
      (CETIS) managed the project from 
      1998-01-01 to 2008-03-31.  Institute
      for the Study of Violent Groups (ISVG) 
      carried the project from 2008-04-01 to 
      2011-10-31.  The National Consortium
      for the Study of Terrorism and
      Responses to Terrorism (START) has
      managed data collection since 
      2011-11-01.  For this variable, 
      partial years are ignored, so 
      \code{methodology} = CEDIS for 
      1998:2007, ISVG for 2008:2011, and 
      START for 2012:2014.  
    }
    \item{incidents}{
      integer number of incidents identified 
      each year.  
      
      NOTE:  
      \code{sum(GTDbyYear[["incidents"]])} =
      146920 = 141966 in the GTD database
      plus 4954 for 1993, for which the
      incident-level data were lost.  
    }
    \item{incidentsus}{
      integer number of incidents identified 
      each year with \code{country_txt} = 
      "United States".  
    }
    \item{suicide}{
      integer number of incidents classified
      as "suicide" by GTD variable 
      \code{suicide} = 1.  For 2007, this 
      is 359, the number reported by 
      \href{https://www.washingtonpost.com/news/monkey-cage/wp/2014/08/11/how-to-fix-the-flaws-in-the-global-terrorism-database-and-why-it-matters/}{Pape et al.}  
      For 2013, it is 624, which is 5 more 
      than the 619 mentioned by Pape et al.  
      Without checking with the SMART 
      project administrators, one might 
      suspect that 5 more suicide incidents 
      from 2013 were found after the data
      Pape et al. analyzed but before the 
      data used for this analysis.  
    }
    \item{suicideus}{
      Number of suicide incidents by year 
      with  \code{country_txt} = 
      "United States". 
    } 
    \item{nkill}{
      number of confirmed fatilities for
      incidents in the given year, including 
      attackers = 
      \code{sum(nkill, na.rm=TRUE)} in the 
      GTD incident data.  
      
      NOTE:  \code{nkill} in the GTD incident
      data includes both perpetrators
      and victims when both are available.  
      It includes one when only one is
      available and is \code{NA} when neither       is available.  However, in most cases, 
      we might expect that the more 
      spectacular and lethal incidents would 
      likely be more accurately reported.  
      To the exent that this is true, it 
      means that when numbers are missing, 
      they are usually zero or small.  This 
      further suggests that the summary 
      numbers recorded here probably 
      represent a slight but not substantive 
      undercount.  
    }
    \item{nkillus}{
      number of U.S. citizens who died as a
      result of incidents for that year = 
      \code{sum(nkillus, na.rm=TRUE)} in the 
      GTD incident data.  (This is subject 
      to the same likely modest undercount 
      discussed with \code{nkill}.)
    }
    \item{nwound}{
      number of people wounded.  (This is
      subject to the same likely modest
      undercount discussed with 
      \code{nkill}.)
    }
    \item{nwoundus}{
      Number of U.S. citizens wounded in
      terrorist incidents for that year = 
      \code{sum(nwoundus, na.rm=TRUE)} in 
      the GTD incident data.  (This is
      subject to the same likely modest
      undercount discussed with 
      \code{nkill}.)
    }
    \item{pNA.kill, pNA.nkillus, 
          pNA.nwound, pNA.nwoundus}{
      proportion of observations by year
      with missing values.  These numbers
      are higher for the early data than 
      more recent numbers.  This is 
      particularly true for \code{nkillus}
      and \code{nwoundus}, which exceed 
      90 percent for most of the period 
      with \code{methodology} = 'PGIS', 
      prior to 1998.  
    }
  }
}
\details{
  As noted with the "description" above, 
  \href{https://www.washingtonpost.com/news/monkey-cage/wp/2014/08/11/how-to-fix-the-flaws-in-the-global-terrorism-database-and-why-it-matters/}{Pape et al.} 
  noted that the GTD reported an increase in
  suicide terrorism of over 70 percent
  between 2007 and 2013, while their \href{https://en.wikipedia.org/wiki/Suicide_Attack_Database}{Suicide Attack Database}
  showed a 19 percent \emph{decrease} over
  the same period.  Pape et al. insisted that
  the most likely explanation for this 
  difference is the change in the 
  organization responsible for managing 
  that data collection from ISVG to START.  
  
  If the issue is restricted to how 
  incidents are classified as "suicide 
  terrorism", this concern does not affect 
  the other variables in this summary.  
  
  However, if it also impacts what 
  incidents are classified as "terrorism", 
  it suggests larger problems.  
}
\source{
  The \href{https://en.wikipedia.org/wiki/Global_Terrorism_Database}{Global Terrorism Database} 
  maintained by the \href{https://en.wikipedia.org/wiki/National_Consortium_for_the_Study_of_Terrorism_and_Responses_to_Terrorism}{National Consortium for the Study of Terrorism and Responses to Terrorism} 
  (START, 2015), downloaded 2015-11-28.  
}
\references{
  Robert Pape, Keven Ruby, Vincent Bauer and 
  Gentry Jenkins, \href{https://www.washingtonpost.com/news/monkey-cage/wp/2014/08/11/how-to-fix-the-flaws-in-the-global-terrorism-database-and-why-it-matters/}{"How to fix
  the flaws in the Global Terrorism Database
  and why it matters"}, 
  \emph{The Washington Post}, August 11, 
  2014 (accessed 2016-01-09).  
}
%\seealso{}
\examples{
data(GTDbyYear)

plot(incidents~year, GTDbyYear, type='l')

plot(nkillus~year, GTDbyYear, type='l')

plot(pNA.nkillus~year, GTDbyYear, type='l')
abline(v=1997.5)
}
\keyword{datasets}
