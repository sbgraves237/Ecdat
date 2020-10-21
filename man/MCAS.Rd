\name{MCAS}
\docType{data}
\alias{MCAS}
\title{The Massachusetts Test Score Data Set }
\description{
  a cross-section from 1997-1998

 \emph{number of observations} :  220

 \emph{observation} :  schools

   \emph{country} :  United States

}
\usage{data(MCAS)}
\format{A dataframe containing :
  \describe{
    \item{code}{district code (numerical)}
    \item{municipa}{municipality (name)}
    \item{district}{district name}
    \item{regday}{spending per pupil, regular}
    \item{specneed}{spending per pupil, special needs}
    \item{bilingua}{spending per pupil, bilingual}
    \item{occupday}{spending per pupil, occupational}
    \item{totday}{spending per pupil, total}
    \item{spc}{students per computer}
    \item{speced}{special education students}
    \item{lnchpct}{eligible for free or reduced price lunch}
    \item{tchratio}{students per teacher}
    \item{percap}{per capita income}
    \item{totsc4}{
      4th grade score (\code{math+english+science})
    }
    \item{totsc8}{
      8th grade score (\code{math+english+science})
    }
    \item{avgsalary}{average teacher salary}
    \item{pctel}{percent English learners}
  }
}
\source{
    Massachusetts Comprehensive Assessment System (MCAS), Massachusetts Department of Education, 1990 U.S. Census.

}
\references{
    Stock, James H.  and  Mark W.  Watson (2003) \emph{Introduction to Econometrics}, Addison-Wesley Educational Publishers, chapter 7.

}
\seealso{
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}}
}


\keyword{datasets}
