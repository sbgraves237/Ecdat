\name{USPS}
\docType{data}
\alias{USPS}
\title{
  US Postal Service 
}
\description{
  Numbers of post offices in the US from 1789 
  to 2020 with their income and expenses in 
  current dollars and proportion of the 
  federal government and of Gross Domestic 
  Product (GDP).  Also includes the number 
  of pieces of mail, numbers of periodicals, 
  pieces and periodicals per person, and 
  cost coverage of periodicals for 
  selected years.  
  
  It would be interesting to find the 
  total value of the subsidies for 
  newspapers and other periodicals as a 
  proportion of the budgets of the USPS
  and the federal government as well as
  of GDP.  That is currently absent from 
  the data consulted to produce this.  
}
\usage{
data(USPS)
}
\format{A \code{\link{data.frame}} containing 232 
      observations on the following variables:
  \describe{
    \item{Year}{ 
      integer: the year:  1789:2020 
    } 
    \item{Income, Expenses}{
      Income and expenses in millions of current 
      dollars, per Historian (2022).
    }    
    \item{Income_pFed, Expenses_pFed}{
      \code{Income} and \code{Expenses} as a 
      proportion of 
      \code{USGDPpresidents[, 'fedReceipts']} and
      \code{USGDPpresidents[, 'fedOutlays']}, 
      respectively.  
    }
    \item{Income_pGDP, Expenses_pGDP}{
      \code{Income} and \code{Expenses} as a 
      proportion of \code{GDP}, per 
      \code{MeasuringWorth}.  
    }
    \item{Income_cap, Expenses_cap}{
      \code{Income} and \code{Expenses} per 
      capita in current dollars = 
      \code{Income} and \code{Expenses} 
      divided by 1000 * 
      \code{USGDPpresidents[, 'population.K']}.
    }
    \item{realIncome_cap, realExpenses_cap}{
      \code{Income} and \code{Expenses} per 
      capita in constant 2012 dollars = 
      \code{Income_cap} and 
      \code{Expenses_cap} divided by 
      \code{USGDPpresidents[, 'GDPdeflator']}.
    }
    \item{postOffices}{
      Number of post offices per Historian 
      (2022).  
    }
    \item{KpopPerPostOffice}{
      US population in thousands per post 
      office:  
      \code{USGDPpresidents[, 'population.K']} 
      divided by \code{postOffices}.  
    }
    \item{piecesOfMail, periodicals}{
      numeric:  Millions of pieces of mail 
      handled and periodicals mailed.  
      "Pieces of mail"" are from Historian 
      (2022).  "Periodicals" are from 
      Historian (2010).  
    } 
    \item{piecesOfMailPerCap, periodicalsPerCap}{
      \code{piecesOfMail} and \code{periodicals}  
      handled per capita (per human in the US) 
      per year.  
    }
    \item{costCoveragePeriodicals}{
      Cost coverage of periodicals, per 
      Historian (2010).  This is available here 
      only since 1960, though Historian (2010) 
      gave a general outline of these numbers.  
      This included saying, "In 1966, the 
      percentage of its own costs covered by 
      second-class mail (or 'cost coverage'),
      including the subsidy, was 35 percent 
      [reported as 36 percent here].  Its real
      coverage was 24 percent." The narrative 
      noted that during parts of the nineteenth 
      century the actual rate was zero.  
      Sometimes it was zero only within county.  
      Sometimes advertising was charged a higher 
      rate than news.  
      
      Other than numbers for the period since 
      1960, we note the coverage in 1951 as 20
      percent, based on the following comment:
      
      "In February 1951, in a special message to
      Congress, President Harry S. Truman argued 
      at length for a rate increase: 'In fiscal 
      year 1952 . . . newspaper and magazine
      publishers will have 200 million dollars 
      -- or 80 percent -- of their postal costs 
      paid for them by the general public.'"
    }
  }
}
\details{
  \code{rownames(USPS) = year}
}
\source{
  Historian (2010-06) 
  \href{https://about.usps.com/who/profile/history/periodicals-postage-history.htm}{Postage Rates for Periodicals: A Narrative History}, accessed 
  2022-04-29.  

  Historian (2022-02) 
  \href{https://about.usps.com/who/profile/history/pieces-of-mail-since-1789.htm}{Pieces of Mail Handled, Number of Post Offices, Income, and Expenses Since 1789}.
}
%\references{}
\author{Spencer Graves}
%\seealso{}
\examples{
##
## plot Expenses as a percent of the 
## federal budget and of GDP 
##
data(USPS)
plot(Expenses_pFed~Year, USPS, type='l')
plot(Expenses_pGDP~Year, USPS, type='l')
plot(100*periodicals/piecesOfMail~Year, 
      USPS, type='l', ylab='', 
      main='periodicals as percent of mail')
      
# Select a year 
# as a charcter string not a number:
USPS['1850',]
}
\keyword{datasets}
