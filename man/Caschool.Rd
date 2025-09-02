\name{Caschool}
\docType{data}
\alias{Caschool}
\title{The California Test Score Data Set }
\description{
  a cross-section from 1998-1999

 \emph{number of observations} :  420

 \emph{observation} :  schools

   \emph{country} :  United States

}
\usage{data(Caschool)}
\format{A dataframe containing :
  \describe{
    \item{distcod}{district code}
    \item{county}{county}
    \item{district}{district}
    \item{grspan}{grade span of district}
    \item{enrltot}{total enrollment}
    \item{teachers}{number of teachers}
    \item{calwpct}{percent qualifying for CalWORKS}
    \item{mealpct}{percent qualifying for reduced-price lunch}
    \item{computer}{number of computers}
    \item{testscr}{
      average test score 
      \code{(read.scr+math.scr)/2}
    }
    \item{compstu}{computer per student}
    \item{expnstu}{expenditure per student}
    \item{str}{student teacher ratio}
    \item{avginc}{district average income}
    \item{elpct}{percent of English learners}
    \item{readscr}{average reading score}
    \item{mathscr}{average math score}
  }
}
\source{
    California Department of Education 
    \url{https://web.archive.org/web/20250823081216/https://www.cde.ca.gov/}.
}
\references{
    Stock, James H.  and  Mark W.  Watson (2003) \emph{Introduction to Econometrics}, Addison-Wesley Educational Publishers, chapter 4--7.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
