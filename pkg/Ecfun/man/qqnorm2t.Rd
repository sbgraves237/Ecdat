\name{qqnorm2t}
\alias{qqnorm2t}
\title{
  Normal Probability Plot with Multiple Lines 
  and Multiple Symbols
}
\description{
  Create a normal probability plot of \code{y} 
  with one line for each level of a 
  \code{\link{factor}} or \code{\link{character}} 
  variable \code{x} and (optionally) different 
  symbols for the different levels of a 
  variable \code{z}.  
  
  To create a normal probability plot with 
  one line for each of multiple \code{y} 
  variables, see \code{\link{qqnorm2s}}.  
  
  To create a normal probability plot with 
  one line and different symbols for each 
  level of a variable \code{z}, see 
  \code{\link{qqnorm2}}.
}
\usage{
qqnorm2t(y, x, z=NULL, data., plot.it=TRUE, 
    datax=TRUE, outnames=NULL, pch=NULL, 
    col=c(1:4, 6), legend.=NULL, ...)
}
\arguments{
  \item{y}{
    a \code{\link{character}} vector of length 
    1 with the name of a column of \code{data.} 
    for which normal probability plots are 
    desired, with one line for each level of 
    \code{x}.
  }
  \item{x}{a \code{\link{factor}} or 
    \code{\link{character}} vector indicating 
    how to split \code{y} for plotting.
  }
  \item{z}{
    A character vector giving the name of a
    column of \code{data.} to indicate 
    different plotting symbols.  
  }
  \item{data.}{
    a \code{\link{data.frame}} with columns 
    named in \code{y}, \code{x}, and \code{z}.
  }
  \item{plot.it}{
    logical:  Should the result be plotted?
  }
  \item{datax}{
    The \code{datax} argument of 
    \code{\link{qqnorm}}:  If TRUE, the
    data are displayed on the horizontal rather
    than the vertical axis.  (The default value
    for \code{datax} is the opposite of that for
    \code{\link{qqnorm}}.)
  }
  \item{outnames}{
    Names for the components of the 
    \code{qqnorm2s} object returned by the
    \code{qqnorm2s} function.  Equal to the 
    levels of \code{x} by default.  
  }
  \item{pch}{
    a named vector of the plotting symbols to be
    used with names corresponding to the levels
    of \code{z}.

    By default, if \code{z} takes levels
    \code{FALSE} and \code{TRUE} (or 0 and 1), 
    pch=c(4, 1) to plot a "x" for \code{FALSE}
    and "o" for \code{TRUE}.

    If \code{z} assumes integer values between 0
    and 255, by default, the symbols are chosen
    as described with \code{\link{points}}.

    Otherwise, by default, \code{z} is coerced to
    \code{\link{character}}, and the result is
    plotted.

    If \code{pch} is provided, it must either
    have names corresponding to levels of
    \code{z}, or \code{z} must be integers
    between 1 and \code{length(pch)}.
  }
  \item{col}{
    A vector indicating the colors corresponding 
    to each element of \code{x}.  Defaults to 
    \code{rep(c(1:4, 6), length=length(x))}, with
    1:4  and 6 being black, red, green, blue, and
    pink.
  }
  \item{legend.}{
    A list with components \code{pch} and
    \code{col} providing information for
    \code{\link{legend}} to identify the plotting
    symbols (\code{pch}) and colors (\code{col}).

    By default, \code{pch = list(x='right',
    legend=names(qq2s[[1]][['pch']]), 
    pch=qq2s[[1]][['pch']])}, where qq2s is
    described below in details.

    Similarly, by default, \code{lines = 
    list(x='bottomright', legend=y,
    lty=1, pch=NA, col=qq2s[[1]][['col']])}.
  }
  \item{\dots}{
    Optional arguments.

    For \code{plot.qqnorm2s}, they are passed to 
    \code{plot}.

    For \code{qqnorm2s}, they are passed to 
    \code{\link{qqnorm2}} and to 
    \code{plot.qqnorm2s}.
  }
}
\details{
  \code{data.} is split by \code{x} and the
  result is passed to \code{qqnorm2s}
}
\value{
  Returns an object of class 
  \code{\link{qqnorm2s}}.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{qqnorm2}}, \code{\link{qqnorm2s}},
  \code{\link{plot}}
}
%\references{}
\examples{
##
## One data.frame
##
tstDF2 <- data.frame(y=1:6, x=c('a','b'), 
    z2=c(TRUE, TRUE, FALSE),
    z3=c('tell', 'me', 'why') )
# produce the object and plot it
Qnt <- qqnorm2t('y', 'x', 'z2', tstDF2)

# plot the object previously created
plot(Qnt)

Qnt0 <- qqnorm2t('y', 'x', data.=tstDF2)
# without z 
qqnorm2t('y', 'x', data.=tstDF2)
}
\keyword{plot}
