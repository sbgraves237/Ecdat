\name{AccountantsAuditorsPct}
\docType{data}
\alias{AccountantsAuditorsPct}
\title{Accountants and Auditors in the US 1850-2016}
\description{
  Accountants and auditors as a percent of the US labor
  force 1850 to 2016 updating the analysis in Wyatt 
  and Hecker (2006).  
}
\usage{data(AccountantsAuditorsPct)}
\format{a numeric vector of length 30 giving the percent of 
  the US labor force in "Accounting and Auditing" each decade 
  from 1850 to 2010 except for 1940 plus each year between 
  2011 and 2016.  
}
\source{
  This is based primarily on data extracted from the 
  \href{https://en.wikipedia.org/wiki/IPUMS}{Integrated 
  Public Use Microdata Series} on 2018-09-01 with 
  the computations documented in a vignette by this 
  title in the \code{Ecfun} package.  
  
  This updates the data on Accountants and Auditors in 
  in Wyatt and Hecker (2006).  They relied primarily on 
  data extracted from the 
  \href{https://en.wikipedia.org/wiki/IPUMS}{Integrated 
  Public Use Microdata Series}.  I did the same but 
  found two problems that seemed to require 
  modifications:  
  
  1.  IPUMS provided no data for 1940.  Wyatt and Hecker 
  (2006) used 
\href{https://www.census.gov/library/publications/1975/compendia/hist_stats_colonial-1970.html}{Historical Statistics of the United 
  States, Colonial Times to 1970, Bicentennial 
  Edition, part 1 (U.S. Department of Commerce, 
  Bureau of the Census, 1975)} for 1910-1940.  
  The current data set uses that source only for 
  1040.  
  
  2.  The IPUMS numbers showed an extreme jump from 
  1850 to 1860 followed by an even more extreme 
  drop to 1870.  The numbers in Sobek (2006) showed
  essentially the same behavior.  Specifically, 
  Sobek (2006) estimated the number of accountants 
  and auditors in the US in those three years as 700, 
  1700, and 1200, and the labor force as 5277000, 
  8160800, and 12004200.  These numbers give 
  accountants and auditors as 0.013, 0.021, and 0.010 
  percent of the labor force, respectively for those 
  three years.  These numbers portray an incredible 
  increase in the employment of accountants and auditors 
  between 1850 and 1860 followed by a shocking decline 
  the following decade. If, however, we swap the 1700 
  and 1200 between 1860 and 1870, the percentages 
  become quite stable: 0.013, 0.015, and 0.014 percent, 
  respectively.  
  
  We use these latter numbers, even thought the 
  uncorrected numbers seem more consistent with 
  the numbers obtained from IPUMS.  
}
\references{
\href{https://www.census.gov/library/publications/1975/compendia/hist_stats_colonial-1970.html}{Historical Statistics of the United 
  States, Colonial Times to 1970, Bicentennial 
  Edition, part 1 (U.S. Department of Commerce, 
  Bureau of the Census, 1975)}

  Steven Ruggles, Sarah Flood, Ronald Goeken, 
    Josiah Grover, Erin Meyer, Jose Pacas, and 
    Matthew Sobek (2018) 
\href{https://doi.org/10.18128/D010.V8.0}{IPUMS 
    USA: Version 8.0 [dataset]. Minneapolis, MN: 
    IPUMS}. 

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
