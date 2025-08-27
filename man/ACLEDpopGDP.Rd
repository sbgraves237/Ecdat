\name{ACLEDpopGDP}
\docType{data}
\alias{ACLEDpopGDP}
\title{ACLED countries and codes with population and GDP}
\description{
  Countries and codes used by the 
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data} 
  project with population and Gross Domestic Project 
  (\code{GDP}) numbers for recent years. Population and 
  \code{GDP} data are from the World Bank when available 
  and from other sources otherwise. When no World Bank 
  data are available, numbers may be reported from the 
  closest year conveniently available, as noted in 
  \code{Comments}; in those cases, the data may not be 
  as accurate as the numbers from the World Bank. 

  NOTE: This code will be offered to the maintainer of the 
  \href{https://search.r-project.org/CRAN/refmans/acled.api/html/00Index.html}{\code{acled.api}}
  package. If they like it, it may not stay in \code{Ecdat}. 
}
\usage{data(ACLEDpopGDP)}
\format{A dataframe with \code{\link{rownames}} = 
        \code{ACLEDcountry} containing :
  \describe{
    \item{ACLEDcountry}{
      A character vector of the country names used by \code{ACLED}
      in the monthly totals of events and deaths between 
      2021-01 and 2024-09 extracted 2024-10-24. 
    }
    \item{ISO3}{
      3-character ISO 3166-1 code for \code{Country}.      
    }
    \item{WBcountry}{
      A character vector of the country names used by the 
      World Bank (\code{WB}) in data extracted 2024-11-06.
    }
    \item{pop2020, pop2021, pop2022, pop2023, pop, 
          GDPpcn2020, GDPpcn2021, GDPpcn2022, GDPpcn2023, 
          GDPpcn, GDPpcp2020, GDPpcp2021, GDPpcp2022, 
          GDPpcp2023, GDPpcp}{
      World Bank population and nominal Gross 
      Domestic Product per capita (\code{GDPpc}) in constant 
      2015 US$ plus GDP per capita, PPP (constant 2021 
      international $) extracted 2024-11-13 for the 
      indicated years unless otherwise specified in 
      "Comments". For country subdivisions like Jersey, 
      the World Bank extract used did not include 
      such numbers. For those "countries", numbers were 
      taken from Wikipedia and assigned to the nearest 
      year in the 2020:2023 range and noted in "Comments".
    }
    \item{Comments}{
      Blank ('') if the data is from the World Bank. 
      Otherwise, this lists the source of the population
      and GDP data, the applicable year, and other 
      anomalies. 
    }
  }
}
\source{
  \href{https://acleddata.com/platform/explorer}{\code{ACLED} Explorer}
  was used 2024-10-24 to download monthly totals between
  2021-01 and 2024-09 of events and death in two files: 
  one for events and another for deaths. Both had data
  on 234 "countries", though some were actually 
  subdivisions. For example, \code{ACLED} "countries" includes
  the \href{https://en.wikipedia.org/wiki/Jersey}{"Bailiwick of Jersey"}, 
  which is a 
  \href{https://en.wikipedia.org/wiki/Crown_Dependencies}{"British Crown"} 
  dependency, and the World Bank does not provide 
  data on them as they do on sovereign countries.

  However, the country names used by \code{ACLED} Explorer do 
  not match the country names used by the World Bank. 
  
  This \code{ACLEDpopGDP} \code{\link{data.frame}} was 
  created to facilitate merging \code{ACLED} data with data
  on population and \code{GDP} ... from the World Bank
  when available and from other sources when not. 
  
  I got most of the ISO 3166-1 3-character country codes
  using \code{\link[Ecfun]{grepInTable}}. That function
  was NOT able to find country codes for the 
  \href{https://en.wikipedia.org/wiki/Caribbean_Netherlands}{Caribbean Netherlands}, 
  \href{https://en.wikipedia.org/wiki/Christmas_Island}{Christmas Island}, 
  \href{https://en.wikipedia.org/wiki/Eswatini}{eSwatini}, and 
  \href{https://en.wikipedia.org/wiki/North_Macedonia}{North Macedonia}, 
  which have 3-letter ISO 3166-1 codes of \code{BES}, 
  \code{CXR}, \code{SWZ}, and \code{MKD}, respectively. 
  
  From the World Bank website, I got something by clicking 
  \href{https://databank.worldbank.org/home.aspx}{\code{DataBank}}. 
  From there, I clicked on 
  \href{https://databank.worldbank.org/indicator/SP.POP.TOTL/1ff4a498/Popular-Indicators}{"Population, total"}. 
  This displayed numbers by country and year from 2008 
  to 2015. I clicked, "Add Time". From there I clicked 
  "Unselect all" then selected 2020, 2021, 2022, and 2023. 
  Then I clicked "x" in the upper right and "Apply Changes". 

  Then I clicked "Add Series". From there I found that 
  many series I did not want were selected, so I clicked 
  "Unselect all", then selected "GDP (constant 2015 US$)" 
  and "Population, total". Then I clicked "x" in the 
  upper right and "Apply Changes" as before. 

  Then I clicked "Download options" and selected "Excel". 
  That downloaded a file named `P_Popular Indicators.xlsx`, 
  which I moved to the working directory, read into 
  R and merged in the obvious way to create most of 
  \code{ACLEDpopGDP}. 
  
  For "Countries" not in the World Bank data I extracted, 
  I got numbers from relevant Wikipedia articles and 
  documented the source in 
  \code{ACLEDpopGDP[, "Comments"]}. 
}
\references{
  \href{https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data}{Armed Conflict Location and Event Data}

  \href{https://databank.worldbank.org/home.aspx}{\code{DataBank}}
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, 
\code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}
\examples{
# Country in World Bank data
ACLEDpopGDP['China', ]

# Country NOT in World Bank data
ACLEDpopGDP['Taiwan', ]

# Partial matching works if unique
ACLEDpopGDP['Czech',]

# Partial matching does NOT work if not unique
ACLEDpopGDP['Saint', ]
# Instead use, e.g., grep
ACLEDpopGDP[grep('Saint', ACLEDpopGDP[, 'ACLEDcountry']), ]

# If you know the ISO 3166-1 3-letter code:
ACLEDpopGDP['CPV'==ACLEDpopGDP[, 'ISO3'], ]
# NOTE: In this example, ACLEDcountry != 
# WBcountry.  

# No NAs in pop
\dontshow{stopifnot(}
all.equal(length(which(is.na(ACLEDpopGDP$pop))), 0)
\dontshow{)}

# Only one NA in GDPpcn and GDPpcp: 
(GDPpNA <- which(is.na(ACLEDpopGDP$GDPpcp)))
(GDPnNA <- which(is.na(ACLEDpopGDP$GDPpcn)))
# Antarctica: 
\dontshow{stopifnot(}
all.equal(ACLEDpopGDP$ACLEDcountry[GDPpNA], 'Antarctica')
\dontshow{)}
\dontshow{stopifnot(
all.equal(ACLEDpopGDP$ACLEDcountry[GDPnNA], 'Antarctica')
)}

ACLEDpopGDP[c('China', 'India'), ]
}
\keyword{datasets}
