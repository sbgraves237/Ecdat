\name{USclassifiedDocuments}
\docType{data}
\alias{USclassifiedDocuments}
\title{
  Official Secrecy of the United States Government 
}
\description{
  Data on classification activity of the United 
  States government.  
  
  Fitzpatrick (2013) notes that the dramatic jump 
  in derivative classification activity
  (\code{DerivClassActivity}) that occurred in 2009
  coincided with "New guidance issued to include
  electronic environment".  Apart from the jump in
  2009, the \code{DerivClassActivity} tended to
  increase by roughly 12 percent per year (with a
  standard deviation of the increase in the natural
  logarithm of \code{DerivClassActivity} of 0.18).  
}
\usage{
data(USclassifiedDocuments)
}
\format{A dataframe containing :
  \describe{
    \item{year}{
      the calendar year 
    }
    \item{OCAuthority}{
      Number of people in the government designated 
      as Original Classification Authorities for 
      the indicated \code{year}.  
    }
    \item{OCActivity}{
      Original classification activity for the
      indicated year:  These are the number of
      documents created with an original
      classification, i.e., so designated by an
      official Original Classification Authority.  
    }
    \item{TenYearDeclass}{
      Percent of \code{OCActivity} covered by the 
      10 year declassification rules.  
    }
    \item{DerivClassActivity}{
      Derivative classification activity for the
      indicated year:  These are the number of
      documents created that claim another document 
      as the authority for classification.  
    }
  }    
}
\details{
  The lag 1 autocorrelation of the first 
  difference of the logarithms of
  \code{DerivClassActivity} through 2008 is 
  \code{-0.52}.  However, because there are only 
  13 numbers (12 differences), this negative
  correlation is not statistically significant.  
}
\source{
  Fitzpatrick, John P. (2013) 
  \emph{Annual Report to the President for 2012},
  United States Information Security Oversight 
  Office, National Archives and Record 
  Administration, June 20, 2013. 
  \href{https://www.archives.gov/isoo/reports}{Information Security Oversight Office (ISOO) of the National Archives.}
%NOTE:  \href failed with this good URL
%2016-12-09:  insert URL w/o \url or href 
%2020-02-09:  URL came up without a warning.  
% This suggests that the problems has been fixed, 
% at least temporarily.  
}
\examples{
##
## 1.  plot DerivClassActivity 
##
plot(DerivClassActivity~year, USclassifiedDocuments)
#  Exponential growth?  

plot(DerivClassActivity~year, USclassifiedDocuments, 
     log='y')
# A jump in 2009 as discussed by Fitzpatrick (2013).  
# Otherwise plausibly a straight line.   

##
## 2.  First difference? 
##
plot(diff(log(DerivClassActivity))~year[-1], 
     USclassifiedDocuments)
# Jump in 2009 but otherwise on distribution 

##
## 3.  autocorrelation?  
##
sel <- with(USclassifiedDocuments, 
            (1995 < year) & (year < 2009) )
acf(diff(log(USclassifiedDocuments$
             DerivClassActivity[sel])))
# lag 1 autocorrelation = (-0.52).  
# However, with only 12 numbers, 
# this is not statistically significant.  
}
\keyword{datasets}
