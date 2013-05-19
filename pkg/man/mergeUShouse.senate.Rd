\name{mergeUShouse.senate}
\alias{mergeUShouse.senate}
\title{
  Expand a dataset on some members of the US Congress to the entire
  membership
}
\description{
  Merge a \code{\link{data.frame}} regarding some members of the US
  Congress with a \code{\link{data.frame}} with general information on
  all members.
}
\usage{
mergeUShouse.senate(x, UScongress=UShouse.senate(),
        newrows="amount0",
        default=list(member=NA, amount=0, vote="notEligible") )
}
\arguments{
  \item{x}{
    a \code{\link{data.frame}} to be merged with \code{UScongress}
  }
  \item{UScongress}{
    a \code{\link{data.frame}} to be merged with \code{x}.
  }
  \item{newrows}{
    name of a \code{logical} column to add that is TRUE for rows added
    to \code{x} and FALSE otherwise.
  }
  \item{default}{
    default values for columns of \code{x} identified by
    \code{regexpr(names(default)[i], tolower(names(x)))}.
  }
}
\details{
  1.  keyx <- with(x, paste(houseSenate, state, District, sep=":"))

  2.  keyy <- with(UScongress(houseSenate, state, District, sep=":"))

  3.  notx <- !is.elememt(keyy, keyx)

  4.  Y <- UScongress[notx, ]

  5.  add default columns to Y

  6.  if(!newrows is not in names(x))x <- cbind(x, newrows=FALSE)

  7.  Y[, newrows] <- TRUE

  8.  rbind(x, Y[c(names(x)])
}
\value{
  a \code{\link{data.frame}} combining \code{x} and \code{UScongress}
  as desired
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{merge}}
  \code{\link{UShouse.senate}}
}
%\references{}
\examples{
tst <- data.frame(Office=factor(rep(c('House', 'Senate'), c(4, 2))),
         State=factor(c('Missouri', 'Minnesota', 'Tennessee',
                        'New York', rep('South Carolina', 2))),
         state=factor(c('MO', 'MN', 'TN', 'NY', 'SC', 'SC')),
         district=as.character(c(4, 1, 8, 18, 2, 3)),
         surname=c('Hartzler', 'Walz', 'Fincher', 'Maloney',
                   'Graham', 'DeMint'),
         givenName=c('Vicky', 'Timothy J.', 'Stephen Lee',
                   'Sean Patrick', 'Lindsey', 'Jim'),
         Party=c('Republican', 'Democrat', 'Republican', 'Democrat',
                 'Republican', 'Democrat'),
         CommitteeMember=rep(c(TRUE, FALSE), c(4, 2)),
         amount=c(5000, 2000, 29500, 1000, 1000, 11500),
         xvote=c('Y', 'N', 'Y', 'Y', 'notEligible', 'notEligible'),
         stringsAsFactors=FALSE )

tst2 <- mergeUShouse.senate(tst)

# A couple of simple tests;  don't test too much,
# because the results of UShouse.senate change,
# and we don't want this test to fail
# due to changes that don't affect Ecdat code

tst3 <- tst2[!tst2$amount0, 1:9]
row.names(tst) <- row.names(tst3)

\dontrun{
all.equal(tst, tst3)
}
# tst3[2] = state = factor with 56 levels,
# and tst[2] only has 5;  compare without this

}
\keyword{manip}
