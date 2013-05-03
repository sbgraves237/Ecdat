\name{mergeVote}
\alias{mergeVote}
\title{
  Merge Roll Call Vote
}
\description{
  Merge roll call vote record with a \code{\link{data.frame}} containing
  other information.  Match on \code{houseSenate} and \code{surname},
  using \code{givenName} only if necessary.
}
\usage{
mergeVote(x, vote, houseSenate="Rep", vote.x)
}
\arguments{
  \item{x}{
    a \code{\link{data.frame}} whose columns include \code{houseSenate},
    \code{surname}, and \code{givenName}.
  }
  \item{vote}{
    a \code{link{data.frame}} with column names which when forced
    \code{\link{tolower}} would match \code{surname}, \code{givenname},
    and \code{vote}.  However, the \code{givenname} may not be
    complete, so use it only if the \code{surname} is not sufficient.
  }
  \item{houseSenate}{
    Either "Rep" or "Sen";  ignored if \code{vote} includes a column
    \code{houseSenate}.
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
}
\details{
  1.  Parse \code{vote.x} to get the name of the column of \code{x} into
  which to write the \code{vote} column of the \code{vote}
  \code{\link{data.frame}}.

  2.  If the \code{vote} \code{\link{data.frame}} contains a column
  \code{houseSenate}, ignore the \code{houseSenate} argument.
  Otherwise, add the argument \code{houseSenate} as a column of
  \code{vote}.

  3. Create \code{keyx <- with(x, paste(houseSenate, surname, sep=":"))}
  and \code{keyy <- with(vote, paste(houseSenate, surname, sep=":"))}.

  4.  Look for \code{keyy} in \code{keyx}.  When a unique match is
  found, transfer the vote the \code{vote} column of \code{x}.  When no
  match is found, record that for a warning and an attribute
  "vote.surnameNotFound" of the output.  When multiple matches are
  found, try to resolve them using givenName.  If that fails, record for
  a warning and an attribute "vote.multipleMatches".
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
votetst <- data.frame(
  surName=c('Smith', 'Jones', 'Graves', 'Jsn', 'Jsn', 'Gay'),
  givenName=c("Sam", "", "", "John", "Linda", ''),
  votex=factor(c('Y', 'N', 'abstain', 'Y', 'Y', '')),
  stringsAsFactors=FALSE )

x1 <- data.frame(
  houseSenate=factor(rep(c("Rep", "Sen"), e=6)),
  surname=rep(c('Jsn', 'Jsn', 'Smith', 'Jones', 'Graves', 'Rx'), 2),
  givenName=rep(c("John D.", "John J.",
    "Samual", "Linda", "Mary", "Susan"), 2),
  don=1:12, stringsAsFactors=FALSE)

x1. <- mergeVote(x1, votetst)

x2 <- cbind(x1, votex=factor( rep(
c('notEligible', 'notEligible', 'Y',
'N', 'abstain', 'notEligible'),
c(1,1,1,1,1,7) ) ) )

attr(x2, 'vote.surnameNotFound') <-
  c('Rep:Jsn, Linda', 'Rep:Gay')
attr(x2, 'vote.multipleMatches') <- "Rep:Jsn, John"

\dontshow{stopifnot(}
all.equal(x1., x2)
\dontshow{)}

}
\keyword{manip}
