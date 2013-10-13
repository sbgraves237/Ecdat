\name{mergeVote}
\alias{mergeVote}
\title{
  Merge Roll Call Vote
}
\description{
  Merge roll call vote record with a \code{\link{data.frame}} containing
  other information.  The \code{vote} records are typically incomplete,
  so match first on \code{houseSenate} and \code{surname}.  If this
  match is incomplete, try using \code{givenName}.  If that fails, try
  \code{state} and \code{district}, which may not always be present in
  \code{vote}.
}
\usage{
mergeVote(x, vote, Office="House", vote.x, check.x=TRUE)
}
\arguments{
  \item{x}{
    a \code{\link{data.frame}} whose columns include \code{Office},
    \code{surname}, and \code{givenName}.
  }
  \item{vote}{
    a \code{\link{data.frame}} with column names which when forced
    \code{\link{tolower}} would match \code{surname}, \code{givenname},
    and \code{vote}.  However, the \code{givenname} may not be
    complete, so use it only if the \code{surname} is not sufficient.
  }
  \item{Office}{
    Either "House" or "Senate";  ignored if \code{vote} includes a column
    \code{Office}.
  }
  \item{vote.x}{
    name of a column of \code{x} containing a vote to be updated with
    the \code{vote} column of the \code{vote} \code{\link{data.frame}}.
    If \code{\link{missing}} and \code{x} has a column with a name
    matching "vote", then \code{vote.x} is that column.  If
    \code{\link{missing}} but \code{x} has no such column, then append a
    column to \code{x} with the name of the \code{vote} column of the
    \code{vote} \code{\link{data.frame}}.
  }
  \item{check.x}{
    logical:  If TRUE, check for rows of \code{x[, vote.x]} that are NOT
    in \code{vote} and throw an error if found.
  }
}
\details{
  1.  Parse \code{vote.x} to get the name of the column of \code{x} into
  which to write the \code{vote} column of the \code{vote}
  \code{\link{data.frame}}.

  2.  If the \code{vote} \code{\link{data.frame}} contains a column
  \code{Office}, ignore the \code{Office} argument.  Otherwise, add the
  argument \code{houseSenate} as a column of \code{vote}.

  3. Create \code{keyx <- with(x, paste(Office, surname, sep=":"))},
  \code{keyx2 <- paste(keyx, givenName, sep=":")}, \code{keyx. <-
  paste(houseSenate, state, district, sep=":")}, and similarly
  \code{keyv}, \code{leyv2}, and \code{keyv.} from \code{vote}.

  4.  Look for \code{keyv} in \code{keyx}.  When a unique match is
  found, transfer the vote the \code{vote} column of \code{x}.  When no
  match is found, try for \code{keyv2} in \code{keyx2} or \code{keyv.}
  in \code{keyx.}  If those fail, print an error message with the
  information from \code{vote} on all failures and ask the user to add
  \code{state} and \code{district} informuation.

  5. \code{if(check.x)}, check for rows in \code{x[, vote.x]} that are
  NOT "notEligible" but are also not in \code{vote}:  Throw an error if
  any are found.
}
\value{
  a \code{\link{data.frame}} with the same columns as \code{x} with its
  vote column modified per the \code{vote} argument.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{mergeUShouse.senate}}
}
%\references{}
\examples{
##
## 1.  Test good cases
##
votetst <- data.frame(
  surName=c('Smith', 'Jones', 'Graves', 'Jsn', 'Jsn', 'Gay'),
  givenName=c("Sam", "", "", "John", "John", ''),
  votex=factor(c('Y', 'N', 'abstain', 'Y', 'Y', 'Y')),
  State=factor(rep(c("CA", "", "SC", "NY"), c(1, 2, 1, 2))),
  district=rep(c("13", "1", "2", "1"), c(1, 2, 2, 1)),
  stringsAsFactors=FALSE )

x1 <- data.frame(
  Office=factor(rep(c("House", "Senate"), e=8)),
  state=rep(c("NY", "SC", "SD", "CA", "AK", "AR", "NY", "NJ"), 2),
  District=rep(c("2", "2", "At Large", "13", "1", "9", "1", "3"), 2),
  surname=rep(c('Jsn', 'Jsn', 'Smith', 'Smith', 'Jones',
       'Graves', 'Rx', 'Agnew'), 2),
  givenName=rep(c("John D.", "John J.",
    "Samual", "Samual", "Mary", "Mary", "Susan", 'Spiro'), 2),
  don=1:16, stringsAsFactors=FALSE)

x1. <- mergeVote(x1, votetst)

x2 <- cbind(x1, votex=factor( rep(
   c('Y', 'notEligible', 'Y', 'N', 'abstain', 'Y', 'notEligible'),
   c(2,1,1,1,1,1,9) ) ) )

\dontshow{stopifnot(}
all.equal(x1., x2)
\dontshow{)}

##
## 2.  Test a case with a vote error in x
##

x1a <- cbind(x1, voterr=rep(
     c('notEligible', 'Y', 'notEligible'), c(7, 1, 8)))

x1a. <- try(mergeVote(x1a, votetst))

\dontshow{stopifnot(}
class(x1a.)=='try-error'
\dontshow{)}

}
\keyword{manip}
