\name{match.data.frame}
\alias{match.data.frame}
\title{
  Identify the row of \code{y} best matching each row of \code{x}
}
\description{
  For each row of \code{x[, by.x]}, find the best matching row of
  \code{y[, by.y]}, with the best match defined by \code{grep.} and
  \code{split}.

  \code{grep.} and \code{split} must either be \code{\link{missing}} or
  have the same length as \code{by.x} and \code{by.y}.  If
  \code{grep.[i]} and \code{split[i]} are NA, do a complete match of
  \code{x[, by.x[i]]} and \code{y[, by.y[i]]}.  Otherwise, for each row
  \code{j}, look for a match for \code{strsplit(x[j, by.x[i]],
    split[i])[[1]][1]} among \code{strsplit(y[, by.y[i]], split[i])}.
  See details.
}
\usage{
match.data.frame(x, y, by, by.x=by, by.y=by, grep., split, sep=':')
}
\arguments{
  \item{x, y}{
    data.frames
  }
  \item{by, by.x, by.y}{
    names of columns of \code{x} and \code{y} to match.
  }
  \item{grep.}{
    a character vector of the type of match for each element of
    \code{by.x} and \code{by.y}.  If \code{NA}, require a perfect
    match.

    Alternatives are \code{\link{grep}} and \code{\link{agrep}}
    to find a match for the first segment in strsplit(x,
    split=split[i]) among any of the segments of strsplit(y,
    split=split[i]).  Use \code{fixed=TRUE} with the calls to these
    functions.

    NOTE:  These alternatives are not examined if a unique match is
    found betweed x[, by.x[is.na(grep.) & is.na(split)]] and the
    corresponding columns of \code{y}.
  }
  \item{split}{
    A character vector of \code{split} characters to pass to
    \code{\link{strsplit}};  \code{\link{strsplit}} is not called if
    \code{is.na(split)}.
  }
  \item{sep}{
    a \code{sep} argument to use with \code{\link{paste}} to produce a
    matching key for the columns of \code{x} and \code{y} for which
    perfect matches are required.  If(missing(sep) &&
    not(missing(grep.))) sep <- ' ' except where grep. = NA.
  }
}
\details{
  1.  Check by.x, by.y, grep. and split.  If((missing(by.x) |
  missing(by.y)) && missing(by)) by <- names(x)

  2.  fullMatch <- (is.na(grep.) & is.na(split)).  Create keyfx and
  keyfy by by pasting columns of x[, by.x[fullMatch]] and y[,
  by.y[fullMatch]].  Also create x. and y. = \code{\link{strsplit}} of
  x[, by.x[!fullMatch]].

  3.  Iterate over rows of \code{x} looking for the best match.  This
  includes an inner loop over columns of x[, by.x[!fullMatch]], stopping
  on the first unique match.  Return (-1) if no unique match is found.
}
\value{
  an integer vector of length nrow(x) containing the index of the best
  matching row of \code{y} or NA if no adequate match was found.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{strsplit}}, \code{\link{is.na}}
  \code{\link{grep}}, \code{\link{agrep}}
  \code{\link{match}}, \code{\link[prodlim]{row.match}},
  \code{\link[plyr]{join}}, \code{\link[plyr]{match_df}}
  \code{\link[TRAMPR]{classify}}
}
%\references{}
\examples{
newdata <- data.frame(state=c("AL", "MI","NY"),
                      surname=c("Rogers", "Rogers", "Smith"),
                      givenName=c("Mike R.", "Mike K.", "Al"),
                      stringsAsFactors=FALSE)
reference <- data.frame(state=c("NY", "NY", "MI", "AL", "NY", "MI"),
                      surname=c("Smith", "Rogers", "Rogers (MI)",
                                "Rogers (AL)", "Smith", 'Jones'),
                      givenName=c("John", "Mike", "Mike", "Mike",
                                "T. Albert", 'Al Thomas'),
                      stringsAsFactors=FALSE)
newInRef <- match.data.frame(newdata, reference,
       grep.=c(NA, 'agrep', 'agrep'))

\dontshow{stopifnot(}
all.equal(newInRef, c(4, 3, 5))
\dontshow{)}
}
\keyword{manip}
