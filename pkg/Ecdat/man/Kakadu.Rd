\name{Kakadu}
\docType{data}
\alias{Kakadu}
\title{Willingness to Pay for the Preservation of the Kakadu National Park }
\description{
  a cross-section

 \emph{number of observations} :  1827

 \emph{observation} :  individuals

   \emph{country} :  Australia

}
\usage{data(Kakadu)}
\format{A dataframe containing :
  \describe{
    \item{lower}{lowerbound of willingness to pay, 0 if observation is left censored}
    \item{upper}{upper bound of willingness to pay, 999 if observation is right censored}
    \item{answer}{an ordered factor with levels nn (respondent answers no, no), ny (respondent answers no, yes or yes, no), yy (respondent answers yes, yes)}
    \item{recparks}{the greatest value of national parks and nature reserves is in recreation activities (from 1 to 5)}
    \item{jobs}{jobs are the most important thing in deciding how to use our natural resources  (from 1 to 5)}
    \item{lowrisk}{development should be allowed to proceed where environmental damage from activities such as mining is possible but very unlikely  (from 1 to 5)}
    \item{wildlife}{it's important to have places where wildlife is preserved  (from 1 to 5)}
    \item{future}{it's important to consider future generations  (from 1 to 5)}
    \item{aboriginal}{in deciding how to use areas such as Kakadu national park, their importance to the local aboriginal people should be a major factor  (from 1 to 5)}
    \item{finben}{in deciding how to use our natural resources such as mineral deposits and forests, the most important thing is the financial benefits for Australia  (from 1 to 5)}
    \item{mineparks}{if areas within natural parks are set aside for development projects such as mining, the value of the parks is greatly reduced  (from 1 to 5)}
    \item{moreparks}{there should be more national parks created from state forests  (from 1 to 5)}
    \item{gov}{the government pays little attention to the people in making decisions  (from 1 to 4)}
    \item{envcon}{the respondent recycles things such as paper or glass and regularly buys unbleached toilet paper or environmentally friendly products ?}
    \item{vparks}{the respondent has visited a national park or bushland recreation area in the previous 12 months ?}
    \item{tvenv}{the respondent watches tv programs about the environment ?  (from 1 to 9)}
    \item{conservation}{the respondent is member of a conservation organization ?}
    \item{sex}{male,female}
    \item{age}{age}
    \item{schooling}{years of schooling}
    \item{income}{respondent's income in thousands of dollars}
    \item{major}{the respondent received the major--impact scenario of the Kakadu conservation zone survey ?}
  }
}
\source{
    Werner, Megan (1999) \dQuote{Allowing for zeros in dichotomous--choice contingent--valuation models}, \emph{Journal of Business and Economic Statistics}, \bold{17(4)}, october, 479--486.

}
\references{
      Journal of Business Economics and Statistics web site : \url{http://amstat.tandfonline.com/loi/ubes20}.
}
\seealso{\code{\link{Index.Source}}, \code{\link{Index.Economics}}, \code{\link{Index.Econometrics}}, \code{\link{Index.Observations}}}


\keyword{datasets}
