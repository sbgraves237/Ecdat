\name{interpPairs}
\alias{interpPairs}
\alias{interpPairs.call}
\alias{interpPairs.function}
\alias{interpPairs.list}
\title{
  interpolate between pairs of vectors in a list
}
\description{
  This does two things:  
  
  \enumerate{
    \item Computes a \code{.proportion} interpolation 
    between \code{pairs} by passing each pair with 
    \code{.proportion} to \code{\link{interpChar}}.   
    \code{\link{interpChar}} does standard linear 
    interpolation with numerics and interpolates 
    based on the number of characters with 
    non-numerics.  
    
    \item Discards rows of interpolants for which 
    \code{.proportion} is outside 
    \code{validProportion}.  If \code{object} is 
    a \code{list}, corresponding rows of other vectors
    of the same length are also discarded.  
    
    NOTE:  There are currently discrepancies between 
    the documentation and the code over defaults when 
    one but not both elements of a pair are provided.  
    The code returns an answer.  If that's not 
    acceptable, provide the other half of the pair.  
    After some experience is gathered, the question 
    of defaults will be revisited and the code or the 
    documentation will change.  
  }  
}
\usage{
interpPairs(object, ...) 
\method{interpPairs}{call}(object, 
    nFrames=1, iFrame=nFrames, 
    endFrames=round(0.2*nFrames), 
    envir = parent.frame(), 
    pairs=c('1'='\\\\.0$', '2'='\\\\.1$', replace0='', 
                replace1='.2', replace2='.3'),     
    validProportion=0:1, message0=character(0), ...)
\method{interpPairs}{function}(object, 
    nFrames=1, iFrame=nFrames, 
    endFrames=round(0.2*nFrames), 
    envir = parent.frame(), 
    pairs=c('1'='\\\\.0$', '2'='\\\\.1$', replace0='', 
                replace1='.2', replace2='.3'),     
    validProportion=0:1, message0=character(0), ...)
\method{interpPairs}{list}(object, 
    .proportion, envir=list(), 
        pairs=c('1'='\\\\.0$', '2'='\\\\.1$', replace0='', 
                replace1='.2', replace2='.3'),     
        validProportion=0:1, message0=character(0), ...)
}
\arguments{
  \item{object}{
    A \code{\link{call}}, \code{\link{function}}, 
    \code{list} or \code{data.frame} with names possibly
    matching \code{pairs[1:2]}.  
    
    When names matching both of \code{pairs[1:2]}, they are 
    converted to potentially common names using 
    \code{sub(pairs[i], pairs[3], ...)}.  When matches are 
    found among the potentially common names, they are     
    passed with \code{.proportion} to 
    \code{\link{interpChar}} to compute an interpolation.  
    The matches are removed and replaced with the 
    interpolant, shortened by excluding any rows for which 
    \code{.proportion} is outside \code{validProportion}.  

    Elements with "common names" that do not have a match 
    are replaced by elements with the common names that 
    have been shortened by omitting rows with 
    \code{.proportion} outside \code{validProportion}.  
    Thus, if \code{x.0} is found without \code{x.1}, 
    \code{x.0} is removed and replaced by \code{x}.  
  }
  \item{nFrames}{
    number of distinct plots to create.
%    The default is computed by the function 
%    \code{\link{nFramesDefault}}.  
  }
  \item{iFrame}{
    integer giving the index of the single frame to create.  Default =
    nFrames.
    
    An error is thrown if both \code{iFrame} and \code{.proportion}
    are not \code{NULL}.  
  }  
  \item{endFrames}{
    Number of frames to hold constant at the end.
  }  
  \item{.proportion}{
    a numeric vector assumed to lie between 0 and 1 specifying how far
    to go from suffixes[1] to suffixes[2].  For example, if \code{x.0}
    and \code{x.1} are found and are numeric, \code{x} = \code{x.0 + 
    .proportion * (x.1 - x.0)}.  Rows of \code{x} and any other element 
    of \code{object} of the same length are dropped for any 
    \code{.proportion} outside \code{validProportion}.
    
    An error is thrown if both \code{iFrame} and \code{.proportion}
    are not \code{NULL}.  
  }
  \item{envir}{
    environment / list to use with code{object}, which can optionally 
    provide other variables to compute what gets plotted;  see the 
    example below using this argument.  
  }
  \item{pairs}{
    a character vector of two regular expressions to identify 
    elements of \code{object} between which to interpolate and 
    three replacements.  
    
    (1) The first of the three replacements is used in 
    \code{\link{sub}} to convert each \code{pairs[1:2]} name 
    found to the desired name of the interpolate.  Common names 
    found are then passed with \code{.proportion} to 
    \code{\link{interpChar}}, which does the actual 
    interpolation. 
    
    (2, 3) \code{interpPairs} also calls 
    \code{checkNames(object, avoid = pairs[c(1, 3, 2, 5)])}.  
    This confirms that \code{object} has \code{\link{names}}, 
    and all such names are unique.  If \code{object} does not 
    have names or has some duplicate names, the 
    \code{\link{make.names}} is called to fix that problem, 
    and any new names that match \code{pairs[1:2]} are modified 
    using \code{\link{sub}} to avoid creating a new match.  
    If the modification still matches \code{pairs[1:2]}, it 
    generates an error.      
  }
  \item{validProportion}{
    Range of values of \code{.proportion} to retain, as noted with the
    discussion of the \code{object} argument.
  }
  \item{message0}{
    a character string passed to \code{\link{interpChar}} 
    to improve the value of diagnostic messages  
  }
  \item{\dots}{ optional arguments for \code{\link{sub}} }
}
\details{
  *** FUNCTION *** 
    
  First \code{interpPairs.function} looks for arguments 
  \code{firstFrame}, \code{lastFrame}, and \code{Keep}.  If 
  any of these are found, they are stored locally and removed
  from the function.  If \code{iFrame} is provided, it is 
  used with with these arguments plus \code{nFrames} and 
  \code{endFrames} to compute \code{.proportion}.  
  
  If \code{.proportion} is outside \code{validProportion}, 
  \code{interpPairs} does nothing, returning 
  \code{enquote(NULL)}.  
  
  If \code{any(.proportion)} is inside \code{validProportion}, 
  \code{interpPairs.function} next uses \code{\link{grep}} to 
  look for arguments with names matching \code{pairs[1:2]}.  
  If any are found, they are passed with \code{.proportion} 
  to \code{\link{interpChar}}.  The result is stored in 
  the modified \code{object} with the common name obtained 
  from \code{sub(pairs[i], pairs[3], ...)}, \code{i} = 1, 2.  

  The result is then evaluated and then returned.  
  
  
  *** LIST *** 

  1.  ALL.OUT:  if(none(0<=.proportion<=1))return 
  'no.op' = list(fun='return', value=NULL) 
  
  2.  FIND PAIRS:  Find names matching \code{pairs[1:2]} 
  using \code{\link{grep}}.  For example, names like 
  \code{x.0} match the default \code{pairs[1]}, 
  and names like \code{x.1} match the default 
  \code{pairs[1]}.  

  3.  MATCH PAIRS:  Use \code{sub(pairs[i], pairs[3], 
  ...)} for i = 1:2, to translate each name matching 
  \code{pairs[1:2]} into something else for 
  matching.  For example, he default \code{pairs} 
  thus translates, e.g., \code{x.0} and 
  \code{x.1} both into \code{x}.  In the output, 
  \code{x.0} and \code{x.1} are dropped, replaced 
  by \code{x} = \code{interpChar(x.0, x.1, 
  .proportion, ...)}.  Rows with 
  \code{.proportion} outside \code{validProportion} 
  are dropped in \code{x}.  Drop similar rows of 
  any numeric or character vector or 
  \code{\link{data.frame}} with the same number of 
  rows as \code{x} or \code{.proportion}.  
  
  4.  Add component \code{.proportion} to \code{envir} 
  to make it available to \code{\link{eval}} any 
  \code{language} component of \code{object} in the next 
  step.  
  
  5.  Loop over all elements of \code{object} to create 
  \code{outList}, evaluating any expressions and 
  computing the desired interpolation using 
  \code{\link{interpChar}}.  Computing \code{xleft} 
  in this way allows \code{xright} to be specified 
  later as \code{quote(xleft + xinch(0.6))}, for 
  example.  This can be used with a call to 
  \code{\link{rasterImageAdj}}.  
  
  6.  Let \code{N} = the maximum number of rows of elements 
  of \code{outList} created by interpolation in the previous 
  step.  If \code{.proportion} is longer, set \code{N} = 
  \code{length(.proportion)}.  Find all vectors and 
  \code{\link{data.frame}}s in \code{outList} with \code{N} 
  rows and delete any rows for which \code{.proportion} is 
  outside \code{validProportion}.  

  7.  Delete the raw pairs found in steps 1-3, retaining the 
  element with the target name computed in steps 4 and 5 
  above.  For other elements of \code{object} modified in the 
  previous step, retain the shortened form.  Otherwise, 
  retain the original, unevaluated element.    
}
\value{
  a \code{list} with elements containing the interpolation results.
}
\author{
  Spencer Graves
}
\seealso{
  \code{\link{interpChar}} for details on interpolation.  
  \code{\link{compareLengths}} for how lengths are checked and 
  messages composed and written.  
%  \code{\link{Animate1.list}}, which uses \code{interpPairs}.  
  \code{\link{enquote}}
}
%\references{}
\examples{
###
###
### 1.  interpPairs.function
###
###

##
## 1.1.  simple 
##
plot0 <- quote(plot(0))
plot0. <- interpPairs(plot0)
# check 
\dontshow{stopifnot(}
all.equal(plot0, plot0.)
\dontshow{)}

##
## 1.2.  no op 
##
noop <- interpPairs(plot0, iFrame=-1)
# check
\dontshow{stopifnot(}
all.equal(noop, enquote(NULL))
\dontshow{)}

##
## 1.3.  a more typical example
## example function for interpPairs 
tstPlot <- function(){
  plot(1:2, 1:2, type='n')
  lines(firstFrame=1:3, 
        lastFrame=4, 
        x.1=seq(1, 2, .5), 
        y.1=x, 
        z.0=0, z.1=1, 
        txt.1=c('CRAN is', 'good', '...'), 
        col='red')
}
tstbo <- body(tstPlot)
iPlot <- interpPairs(tstbo[[2]])
# check 
iP <- quote(plot(1:2, 1:2, type='n'))
\dontshow{stopifnot(}
all.equal(iPlot, iP)
\dontshow{)}

iLines <- interpPairs(tstbo[[3]], nFrames=5, iFrame=2)
# check:  
# .proportion = (iFrame-firstFrame)/(lastFrame-firstFrame)
#  = c(1/3, 0, -1/3)
%# if x.0 = x.1 and y.0 = y.1 by default 
%iL <- quote(lines(x=c(1, 1.5), y=c(1, 1.5), 
%                  z=c(1/3, 0), 
%                  txt=c('CR', '') ))
# if x.0 = 0 and y.0 = 0 by default:  
iL <- quote(linex(x=c(1/3, 0), y=c(1/9, 0), z=c(1/3, 0), 
           tst=c('CR', '')))
##
##**** This example seems to give the wrong answer
##**** 2014-06-03:  Ignore for the moment 
##           
%\dontshow{stopifnot(}
#all.equal(iLines, iL)
%\dontshow{)}

##
## 1.4.  Don't throw a cryptic error with NULL 
##
ip0 <- interpPairs(quote(text(labels.1=NULL)))
  
  
###
###
### 2.  interpPairs.list
###
###

##
## 2.1.  (x.0, y.0, x.1, y.1) -> (x,y)
##
tstList <- list(x.0=1:5, y.0=5:9, y.1=9:5, x.1=9,
                ignore=letters, col=1:5)
xy <- interpPairs(tstList, 0.1)
# check 
xy. <- list(ignore=letters, col=1:5, 
            x=1:5 + 0.1*(9-1:5), 
            y=5:9 + 0.1*(9:5-5:9) )
# New columns, 'x' and 'y', come after 
# columns 'col' and 'ignore' already in tstList 
\dontshow{stopifnot(}
all.equal(xy, xy.)
\dontshow{)}

##
## 2.2.  Select the middle 2:  
##      x=(1-(0,1))*3:4+0:1*0=(3,0)
##
xy0 <- interpPairs(tstList[-4], c(-Inf, -1, 0, 1, 2) )
# check 
xy0. <- list(ignore=letters, col=3:4, x=c(3,0), y=7:6)

\dontshow{stopifnot(}
all.equal(xy0, xy0.)
\dontshow{)}
##
## 2.3.  Null interpolation because of absence of y.1 and x.0  
##
xy02 <- interpPairs(tstList[c(2, 4)], 0.1)
# check 
#### NOT the current default answer;  revisit later.  
xy02. <- list(y=5:9, x=9)

# NOTE:  length(x) = 1 = length(x.1) in testList
%\dontshow{stopifnot(}
#all.equal(xy02, xy02.)
%\dontshow{)}

##
## 2.4.  Select an empty list (make sure this works)
##
x0 <- interpPairs(list(), 0:1)
# check 
x0. <- list()
names(x0.) <- character(0)
\dontshow{stopifnot(}
all.equal(x0, x0.)
\dontshow{)}

##
## 2.5.  subset one vector only 
##
xyz <- interpPairs(list(x=1:4), c(-1, 0, 1, 2))
# check 
xyz. <- list(x=2:3)
\dontshow{stopifnot(}
all.equal(xyz, xyz.)
\dontshow{)}

##
## 2.6.  with elements of class call
##
xc <- interpPairs(list(x=1:3, y=quote(x+sin(pi*x/6))), 0:1)
# check
xc. <- list(x=1:3, y=quote(x+sin(pi*x/6)))
\dontshow{stopifnot(}
all.equal(xc, xc.)
\dontshow{)}

##
## 2.7. text
##
#  2 arguments 
j.5 <- interpPairs(list(x.0='', x.1=c('a', 'bc', 'def')), 0.5)
# check  
j.5. <- list(x=c('a', 'bc', ''))
\dontshow{stopifnot(}
all.equal(j.5, j.5.)
\dontshow{)}

##
##  2.8.  text, 1 argument as a list 
##
j.50 <- interpPairs(list(x.1=c('a', 'bc', 'def')), 0.5)
# check  
\dontshow{stopifnot(}
all.equal(j.50, j.5.)
\dontshow{)}

##
## 2.9.  A more complicated example with elements to eval
##
logo.jpg <- paste(R.home(), "doc", "html", "logo.jpg",
                  sep = .Platform$file.sep)
if(require(jpeg)){
  Rlogo <- readJPEG(logo.jpg)
# argument list for a call to rasterImage or rasterImageAdj   
  RlogoLoc <- list(image=Rlogo,
    xleft.0 = c(NZ=176.5,CH=172,US=171,  CN=177,RU= 9.5,UK= 8),
    xleft.1 = c(NZ=176.5,CH=  9,US=-73.5,CN=125,RU= 37, UK= 2),
    ybottom.0=c(NZ=-37,  CH=-34,US=-34,  CN=-33,RU= 48, UK=47),
    ybottom.1=c(NZ=-37,  CH= 47,US= 46,  CN= 32,RU=55.6,UK=55),
    xright=quote(xleft+xinch(0.6)),
    ytop = quote(ybottom+yinch(0.6)),
    angle.0 =0,
    angle.1 =c(NZ=0,CH=3*360,US=5*360, CN=2*360,RU=360,UK=360)
    )

  RlogoInterp <- interpPairs(RlogoLoc, 
            .proportion=rep(c(0, -1), c(2, 4)) )
# check 
\dontshow{stopifnot(}
all.equal(names(RlogoInterp), 
   c('image', 'xright', 'ytop', 'xleft', 'ybottom', 'angle'))
\dontshow{)} 
# NOTE:  'xleft', and 'ybottom' were created in interpPairs, 
# and therefore come after 'xright' and 'ytop', which were 
# already there.  

##
## 2.10.  using envir
##
  RlogoDiag <- list(x0=quote(Rlogo.$xleft), 
                  y0=quote(Rlogo.$ybottom), 
                  x1=quote(Rlogo.$xright), 
                  y1=quote(Rlogo.$ytop) ) 

  RlogoD <- interpPairs(RlogoDiag, .p=1, 
                      envir=list(Rlogo.=RlogoInterp) ) 
%  RlogoD. <- RlogoInterp[c('xleft', 'ybottom', 'xright', 'ytop')]
%  names(RlogoD.) <- c('x0', 'y0', 'x1', 'y1')
\dontshow{stopifnot(}
all.equal(RlogoD, RlogoDiag)
\dontshow{)}
}
##
## 2.11.  assign;  no interp but should work   
##
tstAsgn <- as.list(quote(op <- (1:3)^2))
intAsgn <- interpPairs(tstAsgn, 1)

# check 
intA. <- tstAsgn 
names(intA.) <- c('X', 'X.3', 'X.2')
\dontshow{stopifnot(}
all.equal(intAsgn, intA.)
\dontshow{)}

#   op <- par(...)
tstP <- quote(op <- par(mar=c(5, 4, 2, 2)+0.1))
tstPar <- as.list(tstP)
intPar <- interpPairs(tstPar, 1)

# check 
intP. <- list(quote(`<-`), quote(op), 
              quote(par(mar=c(5, 4, 2, 2)+0.1)) )
names(intP.) <- c("X", 'X.3', 'X.2')
\dontshow{stopifnot(}
all.equal(intPar, intP.)
\dontshow{)}

intP. <- interpPairs(tstP)
\dontshow{stopifnot(}
all.equal(intP., tstP)
\dontshow{)}

##
## NULL 
## 
\dontshow{stopifnot(}
all.equal(interpPairs(NULL), quote(NULL)) 
\dontshow{)}
}
\keyword{manip}
