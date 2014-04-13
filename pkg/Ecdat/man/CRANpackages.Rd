\name{CRANpackages}
\docType{data}
\alias{CRANpackages}
\title{ Growth of CRAN }
\description{
  Data casually collected on the number of packages on the Comprehensive
  R Archive Network (CRAN) at different dates.
}
\usage{
data(CRANpackages)
}
\format{A \code{data.frame} containing:
  \describe{
    \item{Version}{
      an ordered factor of the R version number primarily in use at the
      time.  This was taken from archives of the major releases at
      \url{https://svn.r-project.org/R/branches/R-1-3-patches/tests/internet.Rout.save},
      ...
      \url{https://svn.r-project.org/R/branches/R-3-1-branch/tests/internet.Rout.save}
    }
    \item{Date}{
      an object of class \code{Date} giving the date on which the count
      of the number of CRAN packages was determined.
    }
    \item{Packages}{
      an integer number of packages on the CRAN mirror checked on the
      indicated \code{Date}.
    }
    \item{Source}{
      A factor giving the source (person) who collected the data.
    }
  }
}
\details{
  This seems to provide the most widely available source for data on the
  growth of CRAN, manually recorded by John Fox and Spencer Graves.  For
  a discussion of these and related data, see Fox (2009).
}
\source{
  John Fox, "Aspects of the Social Organization and Trajectory of the R
  Project", \emph{R Journal}, 1(2), Dec. 2009, 5-13.
  \url{http://journal.r-project.org/archive/2009-2/RJournal_2009-2_Fox.pdf},
  accessed 2014-04-13.
}
%\references{}
%\seealso{}
\examples{
plot(Packages~Date, CRANpackages, log='y')
# almost exponential growth
}
\keyword{datasets}
