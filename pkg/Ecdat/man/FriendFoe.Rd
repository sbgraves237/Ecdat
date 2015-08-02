\name{FriendFoe}
\docType{data}
\alias{FriendFoe}
\title{Data from the Television Game Show Friend Or Foe ? }
\description{
  a cross-section from 2002--03

 \emph{number of observations} :  227

 \emph{observation} :  individuals

   \emph{country} :  United States

}
\usage{data(FriendFoe)}
\format{A dataframe containing :
  \describe{
    \item{sex}{contestant's sex}
    \item{white}{is contestant white ?}
    \item{age}{contestant's age in years}
    \item{play}{contestant's choice : a factor with levels "foe" and "friend". If both players play "friend", they share the trust box, if both play "foe", both players receive zero prize, if one of them play "foe" and the other one "friend", the "foe" player receive the entire trust bix and the "friend" player nothing}
    \item{round}{round in which constestant is eliminated, a factor with levels ("1","2","3")}
    \item{season}{season show, a factor with levels ("1","2")}
    \item{cash}{the amount of cash in the trust box}
    \item{sex1}{partner's sex}
    \item{white1}{is partner white ?}
    \item{age1}{partner's age in years}
    \item{play1}{partner's choice : a factor with levels "foe" and "friend"}
    \item{win}{money won by contestant}
    \item{win1}{money won by partner}
  }
}
\source{
    Kalist, David E. (2004) \dQuote{Data from the Television Game Show "Friend or Foe?"}, \emph{Journal of Statistics Education}, \bold{12(3)}.

}
\references{
      Journal of Statistics Education's data archive : 
      \url{http://www.amstat.org/publications/jse/jse_data_archive.htm}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
