\name{qqnorm2}
\alias{qqnorm2}
\alias{plot.qqnorm2}
\alias{lines.qqnorm2}
\alias{points.qqnorm2}
\title{
  Normal Probability Plot with Multiple Symbols
}
\description{
  Create a normal probability plot with one line and different 
  symbols for the values of another variable, \code{z}.  
  
  \code{qqnorm2} produces an object of class 
  \code{qqnorm2}, whose plot method produces the plot.
  
  To create a normal normal probability plots with multiple 
  lines, see \code{\link{qqnorm2t}} or \code{\link{qqnorm2s}}:x`
  
  \itemize{
    \item \code{\link{qqnorm2s}} produces a plot with multiple 
    lines specified either by different names in a character 
    vector \code{y} or by different \code{\link{data.frame}}s 
    in a list \code{data.}, with different points labeled 
    according to the different levels of \code{z}.  
  
    \item \code{\link{qqnorm2t}} produces a plot with multiple 
    lines with \code{y} split on different levels of \code{x}, 
    optionally with different points labeled according to 
    different levels of \code{z}.  
  }
}
\usage{
qqnorm2(y, z, plot.it=TRUE, datax=TRUE, pch=NULL, ...)
\method{plot}{qqnorm2}(x, y, ...)
\method{lines}{qqnorm2}(x, ...)
\method{points}{qqnorm2}(x, ...)
}
\arguments{
  \item{y}{
    For \code{qnorm2}, \code{y} is a numeric vector for which a normal
    probability plot is desired.

    For \code{plot.qqnorm2}, \code{y} is ignored;  it is included,
    because the generic \code{\link{plot}} function requires it.
  }
  \item{z}{
    A variable to indicate different plotting symbols.
    
    NOTE:  is.logical(z) is replaced by 
    z <- as.character(z). 
    
    Otherwise, pch[z] would delete symbols in pch
    for which z is FALSE and would recycle the 
    remaining symbols.  That would rarely be what
    we want.  
  }
  \item{plot.it}{
    logical:  Should the result be plotted?
  }
  \item{datax}{
    The \code{datax} argument of \code{\link{qqnorm}}:  If TRUE, the
    data are displayed on the horizontal rather than the vertical axis.
    (The default value for \code{datax} is the opposite of that for
    \code{\link{qqnorm}}.)
  }
  \item{x}{
    an object of class \code{qqnorm2}.
  }
  \item{pch}{
    a named vector of the plotting symbols to be used with names
    corresponding to the levels of z.  If \code{pch} is provided, 
    it must either have names corresponding to levels of \code{z}, 
    or \code{z} must be integers between 1 and \code{length(pch)}.

    Otherwise, if \code{z} takes levels \code{FALSE} and 
    \code{TRUE} (or 0 and 1), pch=c(4, 1) to plot an 
    "x" for \code{FALSE} and "o" for \code{TRUE}.

    Or if \code{z} assumes integer values between 0 and 255, by 
    default, the symbols are chosen as described with 
    \code{\link{points}}.
    
    NOTE:  *** points.qqnorm2 may not work properly 
    for z being integer between 0 and 255.  
    lines.qqnorm2 is more likely to work in such 
    cases.  *** No time to fix this as of 
    2018-01-20.  

    Otherwise, by default, \code{z} is coerced to
    \code{\link{character}}, and the result is plotted.
  }
  \item{\dots}{
    Optional arguments.

    For \code{plot.qqnorm2}, they are passed to \code{plot}.

    For \code{qqnorm2}, they are passed to \code{\link{qqnorm}} 
    and to \code{plot.qqnorm2}.
  }
}
\details{
  For \code{qqnorm2}:

  qq1.  q2 <- qqnorm(y, datax=datax, ...)

  qq2.  q2[["z"]] <- z

  qq3.  q2[["pch"]] gets whatever \code{pch} decodes to.

  qq4.  Silently return(list(x, y, z, pch, ...)), where 
  "x" and "y" are as returned by \code{\link{qqnorm}} 
  in step 1 above.  If pch is not provided and z is not 
  logical or positive integers, then z itself will be 
  plotted and "pch" will not be in the returned list.  

  For \code{plot.qqnorm2}:  
  
  plot1.  plot(x\$x, x\$y, type="n", ...) with "..." taking
  precidence over x, where the same plot argument appears
  in both.  
  
  plot2.  if(type \%in\% c('l', 'b', 'c', 'o'))
  lines(x\$x, x\$y, ...)

  plot3.  if(type \%in\% c('p', 'b', 'o')): 
  if(is.null(x\$z))points(x\$x, x\$y, ...)
  else if(is.logical(x\$z))points(x\$x, x\$y, pch=x\$pch[x\$z], ...)
  else if(is.numeric(x\$z) && (min(z0 <- round(x\$z))>0)
  && (max(abs(x\$z-z0))<10*.Machine\$double.eps))
  points(x\$x, x\$y, pch=x\$pch[x\$z], ...)
  else text(x\$x, x\$y, x\$z, ...)

  For \code{lines.qqnorm2}
  lines1.  if(type != 'p')lines(x$x, x$y, ...);  

  lines2.  if(type \%in\% c('p', 'b', 'o'))
  if(is.null(pch))text(x\$x, x\$y, x\$z, ...)
  else if(is.character(pch))text(x\$x, x\$y, x\$phc[x\$z], ...) 
  else points(x\$x, x\$y, pch=x\$pch[x\$z], ...)

  For \code{points.qqnorm2}
  points1.  if(type \%in\% c('p', 'b', 'o'))
  if(is.null(pch))text(x\$x, x\$y, x\$z, ...)
  else if(is.character(pch))text(x\$x, x\$y, x\$phc[x\$z], ...) 
  else points(x\$x, x\$y, pch=x\$pch[x\$z], ...)

  points2.  if(!(type \%in\% c('p', 'n')))
  lines(x$x, x$y, ...)
}
\value{
  \code{qqnorm2} returns a list with components, x, y, z, and pch.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{qqnorm}}, \code{\link{qqnorm2s}}, 
  \code{\link{qqnorm2t}}
  \code{\link{plot}}
  \code{\link{points}}
  \code{\link{lines}}
}
%\references{}
\examples{
##
## a simple test data.frame to illustrate the plot
## but too small to illustrate qqnorm concepts
##
tstDF <- data.frame(y=1:3, z1=1:3, z2=c(TRUE, TRUE, FALSE),
                    z3=c('tell', 'me', 'why'), z4=c(1, 2.4, 3.69) )
# plotting symbols circle, triangle, and "+"
qn1 <- with(tstDF, qqnorm2(y, z1))

# plotting symbols "x" and "o"
qn2 <- with(tstDF, qqnorm2(y, z2))

# plotting with "-" and "+"
qn. <- with(tstDF, qqnorm2(y, z2, pch=c('FALSE'='-', 'TRUE'='+')))

# plotting with "tell", "me", "why"
qn3 <- with(tstDF, qqnorm2(y, z3))

# plotting with the numeric values
qn4 <- with(tstDF, qqnorm2(y, z4))

##
## test plot, lines, points
##
plot(qn4, type='n') # establish the scales
lines(qn4)          # add a line
points(qn4)         # add points

##
## Check the objects created above
##
# check qn1
qn1. <- qqnorm(1:3, datax=TRUE, plot.it=FALSE)
qn1.$xlab <- 'y'
qn1.$ylab <- 'Normal scores'
qn1.$z <- tstDF$z1
qn1.$pch <- 1:3
names(qn1.$pch) <- 1:3
qn11 <- qn1.[c(3:4, 1:2, 5:6)]
class(qn11) <- 'qqnorm2'
\dontshow{stopifnot(}
all.equal(qn1, qn11)
\dontshow{)}

# check qn2
qn2. <- qqnorm(1:3, datax=TRUE, plot.it=FALSE)
qn2.$xlab <- 'y'
qn2.$ylab <- 'Normal scores'
qn2.$z <- tstDF$z2
qn2.$pch <- c('FALSE'=4, 'TRUE'=1)
qn22 <- qn2.[c(3:4, 1:2, 5:6)]
class(qn22) <- 'qqnorm2'
\dontshow{stopifnot(}
all.equal(qn2, qn22)
\dontshow{)}

# check qn.
qn.. <- qqnorm(1:3, datax=TRUE, plot.it=FALSE)
qn..$xlab <- 'y'
qn..$ylab <- 'Normal scores'
qn..$z <- tstDF$z2
qn..$pch <- c('FALSE'='-', 'TRUE'='+')
qn.2 <- qn..[c(3:4, 1:2, 5:6)]
class(qn.2) <- 'qqnorm2'
\dontshow{stopifnot(}
all.equal(qn., qn.2)
\dontshow{)}

# check qn3
qn3. <- qqnorm(1:3, datax=TRUE, plot.it=FALSE)
qn3.$xlab <- 'y'
qn3.$ylab <- 'Normal scores'
qn3.$z <- as.character(tstDF$z3)
qn3.$pch <- as.character(tstDF$z3)
names(qn3.$pch) <- qn3.$pch
qn33 <- qn3.[c(3:4, 1:2, 5:6)]
class(qn33) <- 'qqnorm2'
\dontshow{stopifnot(}
all.equal(qn3, qn33)
\dontshow{)}

# check qn4
qn4. <- qqnorm(1:3, datax=TRUE, plot.it=FALSE)
qn4.$xlab <- 'y'
qn4.$ylab <- 'Normal scores'
qn4.$z <- tstDF$z4
qn44 <- qn4.[c(3:4, 1:2, 5)]
qn44$pch <- NULL
class(qn44) <- 'qqnorm2'
\dontshow{stopifnot(}
all.equal(qn4, qn44)
\dontshow{)}

##
## Test lines(qn4) without z
##
#  just as a test, so this code can be used 
#  in other contexts
qn4. <- qn4
qn4.$z <- NULL
plot(qn4.)


}
\keyword{plot}
