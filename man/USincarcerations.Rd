\name{USincarcerations}
\alias{USincarcerations}
\docType{data}
\title{
  US incarcerations 1925 onward
}
\description{
  Counts of prisoners under the jurisdiction of 
  state and federal correctional authorities in 
  the US.  This does not include jail inmates.
}
\usage{data("USincarcerations")}
\format{
  A data frame with 95 observations on the 
  following 7 variables.
  \describe{
    \item{year}{
      an integer vector giving the year 
      \code{c(1925:2019)}.  
    }
    \item{stateFedIncarcerees}{
      Total number of incarcerees = 
      \code{maleTotal + femaleTotal}. 
    }	
    \item{stateFedIncarcerationRate}{
      incarceration rate = 
      \code{stateFedIncarcerees} per 
      100,000 population.  
    }	
    \item{stateFedMales}{
      Total number of male incarcerees.  
    }	
    \item{stateFedMaleRate}{
      male incarceration rate = 
      \code{maleTotal} per 100,000 males 
      in the US population.  
    }	
    \item{stateFedFemales}{
      Total number of female incarcerees. 
    }	
    \item{stateFedFemaleRate}{
      female incarceration rate = 
      \code{femaleTotal} per 100,000 
      females in the US population.  
    }
  }
}
\details{
  This dataset began as an effort to update 
  \href{https://commons.wikimedia.org/wiki/File:U.S._incarceration_rates_1925_onwards.png}{File:U.S. incarceration rates 1925 \code{onwards.png} on Wikimedia Commons}.  
  Conveniently data on these variables 
  was provided in a table for 1925 to 2014.  
  And a description was given of how to update 
  that table using files \code{p*t03.csv} and 
  \code{p*t05.csv} from 
  \href{https://bjs.ojp.gov/library/publications/prisoners-2019}{Prisoners In 2019}.
  
  An initial rationality check was to compute 
  
  \code{checkTot} <-
  \code{stateFedIncarcerees} - 
  \code{stateFedMales} - 
  \code{stateFedFemales}

  This was 0 except for 1927 and 1973, when it 
  was 637 and 684.  The \code{stateFedFemales} 
  for 1972:1974 was 6269, 6004, 7389.  We 
  replaced 6004 with 6688, which made the 
  \code{checkTot} 0 for 1973.  
  
  Similar checks for 1927 yielded nothing as
  obvious.  However, the
  \code{stateFedIncarcerees} increased 6.9 
  percent in 1926 over 1925, and 12.2 and 
  5.8 percent in the following two years.  
  Subtracting 637 from 109983 for 1927 gave us 
  109346, which reduced the increase to 11.6 
  percent for 1927.  It's no longer the maximum
  annual increase prior to 1975.  

  Next, these numbers were compared with those
  in \code{p19t03.csv} and \code{p19t05.csv}, 
  which include  numbers of incarcerees and 
  rates per 100,000 population for 2009:2019.  
  The numbers were identical for 2009:2011, 
  but there were several differences for the 
  more recent counts.  
  
  For \code{USincarcerations}, we used the 
  numbers from \code{p19t03.csv} and 
  \code{p19t05.csv}, because they seem likely 
  to be more accurate.  
  
  However, these numbers include only people 
  in state and federal prisons.  It excludes
  jails.  
  
  \href{https://bjs.ojp.gov/data/key-statistics}{Key Statistic: Total correctional population} 
  includes a plot of "Total adult 
  correctional population 1980-2016", which
  does include jails.  The data there are
  available as
  \code{Total_correctional_population_counts_by_status.csv}.  Data on these variables covering 
  2008-2018 are available as 
  \code{cpus1718.csv} from "Data tables" at 
  \href{https://bjs.ojp.gov/library/publications/correctional-populations-united-states-2017-2018}{Publication Correctional Populations In The United States, 2017-2018}.  
  The data in \code{cpus1718.csv} is mostly 
  but not entirely identical to "Total adult 
  correctional population 1980-2016" for 
  2008-2016, the period of overlap.  We
  therefore used the older data up to 2007
  and \code{cpus1718.csv} for 2008-2018.  
  
  Actual analysis of the jail data is left
  for another project.  
}
\source{
  Data from 1925 to 2014 from 
  \href{https://commons.wikimedia.org/wiki/File:U.S._incarceration_rates_1925_onwards.png}{File:U.S. incarceration rates 1925 \code{onwards.png} on Wikimedia Commons}, accessed 2020-11-23.  
  
  The primary source for the more recent data are 
  files \code{p*t03.csv} and \code{p*t05.csv} from
  \href{https://bjs.ojp.gov/library/publications/prisoners-2019}{Prisoners In 2019}, accessed 2020-11-23.  
  
    Data on jails and community supervision 
  dating back to 1980 are available in 
  \href{https://bjs.ojp.gov/data/key-statistics}{Key Statistic: Total correctional population} 
  with data on the most recent years 
  available from 
  \href{https://bjs.ojp.gov/library/publications/correctional-populations-united-states-2017-2018}{Publication Correctional Populations In The United States, 2017-2018}.  
  
  Some time in 2021 or later more recent data
  should become available.  When that happens, 
  it may be desired to update this table to
  include those numbers -- and check for any
  revisions of earlier numbers.  
}
\references{
  \href{https://en.wikipedia.org/wiki/United_States_incarceration_rate}{United States incarceration rate}.  
}
\examples{
data(USincarcerations)

matplot(USincarcerations[1],
  0.001*USincarcerations[c(3, 5, 7)], type='l', 
  xlab='', ylab='incarceration rate (\%)')
abline(h=0.5, lty='dotted', col='gray')
lbl <- paste("US incarceration rate", 
  '(percent of the population)', sep='\n')
text(1955, 0.75, lbl)
text(2007, 0.86, 'male', col=2)
text(2007, 0.15, 'female', col=3)
}
\keyword{datasets}
