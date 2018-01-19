\name{qqnorm2s}
\alias{qqnorm2s}
\alias{plot.qqnorm2s}
\title{
  Normal Probability Plot with Multiple Lines 
  and Multiple Symbols
}
\description{
  Create a normal probability plot with one 
  line for each \code{y} variable or each 
  \code{\link{data.frame}} in a list \code{data.}
  with different plotting symbols for the values 
  of \code{z}.  
  
  To create a normal probability plot with one 
  \code{y} variable split on a \code{link{factor}}
  or \code{\link{character}} variable \code{x}, 
  see \code{\link{qqnorm2t}}.  

  \code{qqnorm2s} produces an object of class
  \code{qqnorm2s}, whose plot method produces 
  the plot.
}
\usage{
qqnorm2s(y, z=NULL, data., plot.it=TRUE, datax=TRUE,
    outnames=NULL, pch=NULL, col=c(1:4, 6), 
    legend.=NULL, ...)
\method{plot}{qqnorm2s}(x, y, ...)
}
\arguments{
  \item{y}{
    a \code{\link{character}} vector of names 
    of columns of \code{data.} for which normal 
    probability plots are desired.  \code{data.}
    is either a \code{\link{data.frame}} or a
    list of \code{\link{data.frame}}s of the same
    length as \code{y}, with y[i] being the name
    of a column of the \code{\link{data.frame}} 
    \code{data.[[i]]}.  \code{z} is a similar
    character vector of names of columns of
    \code{data.}, which identify symbols for
    plotting different points in a normal
    probability plot.  
    
    The lengths of \code{y}, and \code{z} must
    match the number of \code{data.frame}s in
    \code{data.};  if not, the lengths of the
    shorter are replicated to the length of the
    longest before computations begin.

    For \code{plot.qqnorm2s}, \code{y} is ignored;
    it is included, because the generic 
    \code{\link{plot}} function requires it.
  }
  \item{z}{
    A character vector giving the names of
    columns of \code{data.} to
    indicate different plotting symbols.  \code{z} should be the same
    length as \code{y} and must equal the number
    of data.frames in the list \code{data.} of
    data.frames.  If not, the shorter are
    replicated to the length of the longer.
  }
  \item{data.}{
    a \code{\link{data.frame}} or a list of 
    data.frames with columns named in \code{y}
    and \code{z}.
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
    \code{qqnorm2s} function.
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
    to each element of \code{y}.  Defaults to 
    \code{rep(c(1:4, 6), length=length(y))}, with
    1:4  and 6 being black, red, green, blue, and
    pink.
  }
  \item{x}{
    an object of class \code{qqnorm2}.
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
  For \code{qqnorm2s}:

  1.  Create qq2s = a list of objects of class
  \code{qqnorm2}

  2.  Add \code{legend.} to \code{qq2s}.

  3.  class(qq2s) <- 'qqnorm2s'

  4.  if(plot.it)plot(qq2s, ...)

  5.  Silently return(qq2s).

  For \code{plot.qqnorm2s}, create a plot with
  one line for each variable named in \code{y}.
}
\value{
  \code{qqnorm2s} returns a named list with
  components of class \code{qqnorm2} with names 
  = \code{y} with each component having an
  additional component \code{col} plus one called
  "legend.".
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{qqnorm2}}
  \code{\link{plot}}
}
%\references{}
\examples{
##
## One data.frame
##
tstDF2 <- data.frame(y=1:3, y2=3:5, 
    z2=c(TRUE, TRUE, FALSE),
    z3=c('tell', 'me', 'why'), 
    z4=c(1, 2.4, 3.69) )
# produce the object and plot it
Qn2 <- qqnorm2s(c('y', 'y2'), 'z2', tstDF2)

# plot the object previously created
plot(Qn2)

# Check the object
qy <- with(tstDF2, qqnorm2(y, z2, type='b'))
qy$col <- 1
qy2 <- with(tstDF2, qqnorm2(y2, z2, type='b'))
qy2$col <- 2
legend. <- list(
  pch=list(x='right', 
      legend=c('FALSE', 'TRUE'),
      pch=c('FALSE'=4, 'TRUE'= 1)),
  col=list(x='bottomright', 
      legend=c('y', 'y2'), lty=1, col=1:2))
Qn2. <- list(y=qy, y2=qy2, legend.=legend.)
class(Qn2.) <- 'qqnorm2s'

\dontshow{stopifnot(}
all.equal(Qn2, Qn2.)
\dontshow{)}
##
## Two data.frames
##
tstDF2b <- tstDF2
tstDF2b$y <- c(0.1, 0.1, 9)
Qn2b <- qqnorm2s('y', 'z2', 
    list(tstDF2, tstDF2b), 
    outnames=c('ok', 'oops'), log='x' )
##
## Split one data.frame 
##
tstDF2. <- rbind(cbind(tstDF2, z1=1), 
                 cbind(tstDF2b, z1=2) )
Qn2. <- qqnorm2s('y', 'z1', tstDF2.)
# Plot has only one line, because only 1 y variable.  
##
## Two data.frames without z
##
Qn2.0 <- qqnorm2s('y', 
    data.=list(tstDF2, tstDF2b), 
    outnames=c('ok', 'oops'), log='x' )
}
\keyword{plot}
