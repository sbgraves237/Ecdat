\name{UnempDur}
\docType{data}
\alias{UnempDur}
\title{Unemployment Duration }
\description{
  Journal of Business Economics and Statistics web site : \url{https://amstat.tandfonline.com/loi/ubes20}

 \emph{number of observations} :  3343

}
\usage{data(UnempDur)}
\format{A time series containing :
  \describe{
    \item{spell}{length of spell in number of two-week intervals}
    \item{censor1}{= 1 if re-employed at full-time job}
    \item{censor2}{= 1 if re-employed at part-time job}
    \item{censor3}{1 if re-employed but left job: pt-ft status unknown}
    \item{censor4}{1 if still jobless}
    \item{age}{age}
    \item{ui}{= 1 if filed UI claim}
    \item{reprate}{eligible replacement rate}
    \item{disrate}{eligible disregard rate}
    \item{logwage}{log weekly earnings in lost job (1985\$)}
    \item{tenure}{years tenure in lost job}
  }
}
\source{
    McCall, B.P. (1996) \dQuote{Unemployment Insurance Rules, Joblessness, and Part-time Work}, \emph{Econometrica}, \bold{64}, 647--682.

}
\references{
    Cameron, A.C.  and  P.K.  Trivedi (2005) \emph{Microeconometrics : methods and applications}, Cambridge, pp. 603--8, 632--6, 658--62, 671--4 and 692.

}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}},

 \code{\link{Index.Time.Series}}}


\keyword{datasets}
