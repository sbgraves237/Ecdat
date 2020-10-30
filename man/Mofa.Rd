\name{Mofa}
\docType{data}
\alias{Mofa}
\title{
  International Expansion of U.S. MOFAs 
  (majority--owned Foreign Affiliates in Fire 
  (finance, Insurance and Real Estate) }
\description{
  a cross-section from 1982

 \emph{number of observations} :  50

 \emph{observation} :  country

   \emph{country} :  United States

}
\usage{data(Mofa)}
\format{A dataframe containing :
  \describe{
    \item{capexp}{
      capital expenditures made by the MOFAs of 
      nonbank U.S. corporations in finance, 
      insurance and real estate. Source: 
      "U.S. Direct Investment Abroad: 1982 
      Benchmark Survey data." Table III.C 6.
    }
    \item{gdp}{
      gross domestic product. Source: "World 
      Bank, World Development Report 1984." Table 
      3. (This variable is  scaled by a factor of 
      1/100,000)
    }
    \item{sales}{sales made by the majority owned foreign affiliates of nonbank U.S. parents in finance, insurance and real estate. Source: "U.S. Direct Investment Abroad: 1982 Benchmark Survey Data." Table III.D 3. (This variable is scaled by a factor of 1/100)}
    \item{nbaf}{the number of U.S. affiliates in the host country. Source: "U.S. Direct Investment Abroad: 1982 Benchmark Survey Data." Table 5. (This variable is scaled by a factor of 1/100)}
    \item{netinc}{net income earned by MOFAs of nonbank U.S. corporations operating in the nonbanking financial sector of the host country. Source: "U.S. Direct Investment Abroad: 1982 Benchmark Survey Data." Table III.D 6.(This variable is scaled by a factor of 1/10)}
  }
}
\source{
    Ioannatos, Petros E. (1995) \dQuote{Censored regression estimation under unobserved heterogeneity : a stochastic parameter approach}, \emph{Journal of Business and Economics Statistics}, \bold{13(3)}, July, 327--335.

}
\references{
      Journal of Business Economics and Statistics web site : \url{https://amstat.tandfonline.com/loi/ubes20}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
