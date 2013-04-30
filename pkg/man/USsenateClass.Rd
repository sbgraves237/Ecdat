\name{USsenateClass}
\alias{USsenateClass}
\title{
  Election Class given state and surname of a US Senator
}
\description{
  For all indiviuals in \code{x} with \code{houseSenate} == "Sen", look
  up their \code{state} and \code{surname} in the reference table
  \code{senate} and return their \code{Class}.  For indivuals not found
  in \code{senate}, return \code{x[["District"]]}.
}
\usage{
USsenateClass(x, senate=readUSsenate())
}
\arguments{
  \item{x}{
    \code{\link{data.frame}} with character or factor columns
    \code{houseSenate}, \code{state}, \code{surname}, and
    \code{District}.
  }
  \item{senate}{
    \code{\link{data.frame}} as returned by \code{\link{readUSsenate}}.
  }
}
\details{
  The current algorithm may fail if both senators in a \code{state} have
  the same \code{surname}.
}
%\source{ }
\value{
  a character vector with length = \code{nrow(x)} containing the desired
  \code{Class} for all US Senators found in \code{senate} and the
  content of \code{x[["District"]]} for all others.
}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{readUSsenate}}
}
\examples{
tst <- data.frame(houseSenate=c("Rep", "Sen", "Sen"),
                  state=rep("SC", 3),
                  surname=c("Jones", "DeMint", "Graham"),
                  District=c("9", "", ""))
tst2 <- USsenateClass(tst)
}
\keyword{manip}