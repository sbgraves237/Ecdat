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
    \item{sector}{firm's main industry sector, one of aero (aerospace), chem (chemistry), comput (computer), drugs, elec (electricity), food, fuel (fuel and mining), glass, instr (instruments), machin (machinery), metals, other, paper, soft (software), motor (motor vehicules) }
    \item{geo}{geographic area, one of eu (European Union), japan, usa, rotw (rest of the world)}
    \item{patent}{numbers of European patent applications}
    \item{rdexp}{log of R\&D expenditures}
    \item{spil}{log of spillovers}
  }
}
\source{
    Cincer, Michele (1997) \dQuote{Patents, R \& D and technological spillovers at the firm level : some evidence from econometric count models for panel data}, \emph{Journal of Applied Econometrics}, \bold{12(3)}, may--june, 265--280.

}
\references{
      Journal of Applied Econometrics data archive : \url{http://jae.wiley.com/jae/}.
    Verbeek, Marno (2004) \emph{A guide to modern econometrics}, John Wiley and Sons, \url{http://www.econ.kuleuven.ac.be/GME}, chapter 7.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
