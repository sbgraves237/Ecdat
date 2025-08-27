\name{PatentsHGH}
\docType{data}
\alias{PatentsHGH}
\title{Dynamic Relation Between Patents and R&D }
\description{
  a panel of 346 observations  from 1975  to 1979 

 \emph{number of observations} :  1730

 \emph{observation} :  production units

   \emph{country} :  United States

}
\usage{data(PatentsHGH)}
\format{A dataframe containing :
  \describe{
    \item{obsno}{firm index}
    \item{year}{year}
    \item{cusip}{ 
      Compustat's identifying number for the 
      firm (Committee on Uniform Security 
      Identification Procedures number)
    }
    \item{ardsic}{
      a two-digit code for the applied R&D 
      industrial classification (roughly that 
      in Bound, Cummins, Griliches, Hall, and 
      Jaffe, in the Griliches R&D, Patents, 
      and Productivity volume)
    }
    \item{scisect}{
      is the firm in the scientific sector ?
    }
    \item{logk}{
      the logarithm of the book value of capital 
      in 1972.
    }
    \item{sumpat}{
      the sum of patents applied for between 
      1972-1979. 
    }
    \item{logr}{ 
      the logarithm of R&D spending during the 
      year (in 1972 dollars)
    }
    \item{logr1}{ 
      the logarithm of R&D spending (one year lag)
    }
    \item{logr2}{ 
      the logarithm of R&D spending (two years lag)
    }
    \item{logr3}{ 
      the logarithm of R&D spending (three years lag)
    }
    \item{logr4}{ 
      the logarithm of R&D spending (four years lag)
    }
    \item{logr5}{ 
      the logarithm of R&D spending (five years lag)
    }
    \item{pat}{
      the number of patents applied for during the 
      year that were eventually granted
    }
    \item{pat1}{the number of patents (one year lag)}
    \item{pat2}{the number of patents (two years lag)}
    \item{pat3}{
      the number of patents (three years lag)
    }
    \item{pat4}{
      the number of patents (four years lag)
    }
  }
}
\source{
    Hall, Bronwyn, Zvi Griliches and Jerry Hausman (1986) \dQuote{Patents and R&D: Is There a Lag?}, \emph{International Economic Review}, \bold{27}, 265-283.

}
\references{
    Cameron, A.C.  and   Trivedi P.K. (1998) \emph{Regression analysis of count data}, Cambridge University Press, \url{https://cameron.econ.ucdavis.edu/racd/racddata.html}, chapter 9.

    Cameron, A.C.  and  P.K.  Trivedi (2005) \emph{Microeconometrics : methods and applications}, Cambridge, pp. 792--5.

}
\seealso{
  \code{\link{PatentsRD}}, 
  \code{\link{Index.Source}},
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}},
 \code{\link{Index.Time.Series}}
}


\keyword{datasets}
