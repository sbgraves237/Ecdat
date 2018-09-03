\name{AccountantsAuditorsPct}
\docType{data}
\alias{AccountantsAuditorsPct}
\title{Accountants and Auditors in the US 1850-2016}
\description{
  Accountants and auditors as a percent of the US labor
  force 1850 to 2016, according to data extracted from the 
  \href{https://en.wikipedia.org/wiki/IPUMS}{Integrated 
  Public Use Microdata Series} on 2018-09-01 for numbers
  since 1880 and modifications of Sobek (2006) for 1850 
  through 1870.  
}
\usage{data(AccountantsAuditorsPct)}
\format{a numeric vector of length 30 giving the percent of 
  the US labor force in "Accounting and Auditing" each decade 
  from 1850 to 2010 except for 1940 plus each year between 
  2011 and 2016.  
}
\source{
  Computations documented in a vignette by this title in 
  the Ecfun package, based on data extracted from the 
  \href{https://en.wikipedia.org/wiki/IPUMS}{Integrated 
  Public Use Microdata Series} on 2018-09-01.  
  
  This updates the data on Accountants and Auditors in 
  in Wyatt and Hecker (2006) using the same data that 
  they used for 1880 to 2016 with modifications of 
  Sobek (2006) for 1850 to 1870.  
  
  Specifically, Sobek (2006) estimated the number of 
  Accountants and Auditors in the US in those three 
  years as 700, 1700, and 1200, and the labor force as 
  5277000, 8160800, and 12004200 for those years.  
  These numbers give Accountants and Auditors as 0.013, 
  0.021, and 0.010 percent of the labor force, 
  respectively for those three years.  These numbers 
  portray an incredible increase in the employment of 
  accountants and auditors between 1850 and 1860 
  followed by a shocking decline the following decade. 
  
  If, however, we swap the 1700 and 1200 betweeen 
  1860 and 1870, the percentages become quite stable: 
  0.013, 0.015, and 0.014 percent, respectively.  
  
  We use these latter numbers.  
}
\references{
  Matthew Sobek (2006) Chapter Ba. "Labor Occupations" 
    in Susan B. Carter, ed., 
\href{https://en.wikipedia.org/wiki/Historical_Statistics_of_the_United_States}{
    Historical Statistics of the United States, 
    Cambridge U. Pr.}
  
  Ian D. Wyatt and Daniel E. Hecker (2006) 
\href{https://www.bls.gov/mlr/2006/03/art3full.pdf}{
    "Occupational changes during the 20th century", 
    Monthly Labor Review, March 2006, pp. 35-57}
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}
\examples{
data(AccountantsAuditorsPct)
plot(names(AccountantsAuditorsPct), AccountantsAuditorsPct, 
  type='l', log='y', cex.axis=1.8)
# for the version of this contributed to Wikimedia Commons  
}
\keyword{datasets}
