\name{Grunfeld}
\docType{data}
\alias{Grunfeld}
\title{Grunfeld Investment Data }
\description{
  a panel of 20 annual observations from 
  1935 to 1954 on each of 10 firms.  

 \emph{number of observations} :  200

 \emph{observation} :  production units

   \emph{country} :  United States

}
\usage{data(Grunfeld)}
\format{A dataframe containing :
  \describe{
    \item{firm}{observation}
    \item{year}{date}
    \item{inv}{gross Investment}
    \item{value}{value of the firm}
    \item{capital}{stock of plant and equipment}
  }
}
\details{
  There are several versions of these data.  
  
  \code{\link[systemfit]{GrunfeldGreene}} is "A
  data frame containing 20 annual observations on 
  3 variables for 5 firms."  That dataset 
  reportedly contains errors but is maintained
  in that way to avoid breaking the code of 
  others who use it.  That help file also provides 
  a link to the corrected version.  
  
  See also 
  \href{https://pages.stern.nyu.edu/~wgreene/Text/tables/TableF13-1.txt}{for a version with only 5 firms}.  
}
\source{
    Moody's Industrial Manual, Survey of Current Business.

}
\references{
    Greene, W.H. (2003) \emph{Econometric Analysis}, Prentice Hall, Table F13.1.

    Baltagi, Badi H. (2003) \emph{Econometric analysis of panel data}, John Wiley and sons, \url{https://www.wiley.com/legacy/wileychi/baltagi/}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},
\code{\link[systemfit]{GrunfeldGreene}}, 

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
