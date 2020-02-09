\name{PatentsRD}
\docType{data}
\alias{PatentsRD}
\title{Patents, R\&D and Technological Spillovers for a Panel of Firms }
\description{
  a panel of 181 observations  from 1983  to 1991

 \emph{number of observations} :  1629

 \emph{observation} :  production units

   \emph{country} :  world

}
\usage{data(PatentsRD)}
\format{A dataframe containing :
  \describe{
    \item{year}{year}
    \item{fi}{firm's id}
    \item{sector}{
      firm's main industry sector, one of 
      \code{aero} (aerospace), \code{chem} (chemistry),
      \code{comput} (computer), \code{drugs}, 
      \code{elec} (electricity), \code{food}, 
      \code{fuel} (fuel and mining), \code{glass}, 
      \code{instr} (instruments), \code{machin} 
      (machinery), \code{metals}, \code{other}, 
      \code{paper}, \code{soft} (software), 
      \code{motor} (motor vehicles) }
    \item{geo}{
      geographic area, one of \code{eu} (European 
      Union), \code{japan}, \code{usa}, \code{rotw} 
      (rest of the world)
    }
    \item{patent}{numbers of European patent applications}
    \item{rdexp}{log of R\&D expenditures}
    \item{spil}{log of spillovers}
  }
}
\source{
    Cincer, Michele (1997) \dQuote{Patents, R \& D and technological spillovers at the firm level : some evidence from econometric count models for panel data}, \emph{Journal of Applied Econometrics}, \bold{12(3)}, May--June, 265--280.
}
\references{
      Journal of Applied Econometrics data archive : \url{http://qed.econ.queensu.ca/jae/}.
    Verbeek, Marno (2004) \emph{A Guide to Modern Econometrics}, John Wiley and Sons, chapter 7.

}
\seealso{
  \code{\link{PatentsHGH}}, 
  \code{\link{Index.Source}}, 
  \code{\link{Index.Economics}}, 
  \code{\link{Index.Econometrics}}, 
  \code{\link{Index.Observations}},
   \code{\link{Index.Time.Series}}
}


\keyword{datasets}
