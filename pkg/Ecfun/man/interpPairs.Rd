\name{interpPairs}
\alias{interpPairs}
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
    
    \item Discards rows of interpolants and other 
    vectors of the same length for which 
    \code{.proportion} is outside 
    \code{validProportion}.    
  }  
}
\usage{
interpPairs(object, .proportion, envir=list(), 
        pairs=c('1'='\\\\.0$', '2'='\\\\.1$', replace0='', 
                replace1='.2', replace2='.3'),     
        validProportion=0:1, Source=character(0), ...)
}
\arguments{
  \item{object}{
    A \code{list} or \code{data.frame} with column names possibly
    matching \code{suffixes}.  When one is found without the other, the
    one is returned with a warning, without its suffix.  Thus, if
    \code{x.0} is found without \code{x.1}, \code{x.0} is returned as an
    element of the output \code{list}.
  }
  \item{.proportion}{
    a numeric vector assumed to lie between 0 and 1 specifying how far
    to go from suffixes[1] to suffixes[2].  For example, if \code{x.0}
    and \code{x.1} are found and are numeric, \code{x} = \code{x.0 + 
    .proportion * (x.1 - x.0)}.  Rows of \code{x} and any other element 
    of \code{object} of the same length are dropped for any 
    \code{.proportion} outside \code{validProportion}.
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
    
    (1) The first is used in \code{\link{sub}} to convert each
    \code{pairs[1:2]} name found to the desired name of the 
    interpolate.  Common names found are then passed with 
    \code{.proportion} to \code{\link{interpChar}}, which 
    does the actual interpolation. 
    
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
  \item{Source}{
    a character string passed to \code{\link{interpChar}} 
    to improve the value of diagnostic messages  
  }
  \item{\dots}{ optional arguments for \code{\link{sub}} }
}
\details{
  1.  if(none(0<=.proportion<=1))return no op = 
  list(fun='return', value=NULL) 
  
  2.  Find names matching \code{pairs[1:2]} using 
  \code{\link{grep}}.  For example, names like 
  \code{x.0} match the default \code{pairs[1]}, 
  and names like \code{x.1} match the default 
  \code{pairs[1]}.  

  3.  Use \code{sub(pairs[i], pairs[3], ...)} for 
  i = 1:2, to translate each name matching 
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
  \code{outList}, evaluating any expressions.  Retain 
  the unevaluated form 
  
  
  As pairs 
  are copied or evaluated, compute the desired 
  interpolation using \code{\link{interpChar}}.  Computing 
  \code{xleft} in this way allows \code{xright} to be 
  specified later as \code{quote(xleft + xinch(0.6))}, for 
  example.  This can be used with a call to 
  \code{\link{rasterImageAdj}}.  
  
  6.  Let \code{N} = the maximum number of rows of elements 
  of \code{outList} created by interpolation in the previous 
  step.  If \code{.proportion} is longer, set \code{N} = 
  \code{length(.proportion)}.  Find all vectors and 
  \code{\link{data.frame}}s in \code{outList} with \code{N} 
  rows and delete any rows for which \code{.proportion} is 
  outside \code{validProportion}.  

  6.  Delete the raw pairs found in steps 1-3, retaining the 
  element with the target name computed in steps 4 and 5 
  above.
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
  \code{\link{animate1.list}}, which uses \code{interpPairs}.  
}
%\references{}
\examples{
##
## 1.  (x.0, y.0, x.1, y.1) -> (x,y)
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
## 2.  Select the middle 2 
##
xy0 <- interpPairs(tstList[-4], c(-Inf, -1, 0, 1, 2) )
# check 
xy0. <- list(ignore=letters, col=3:4, x=3:4, y=7:6)

\dontshow{stopifnot(}
all.equal(xy0, xy0.)
\dontshow{)}
##
## 3.  Null interpolation because of absence of y.1 and x.0  
##
xy02 <- interpPairs(tstList[c(2, 4)], 0.1)
# check 
xy02. <- list(y=5:9, x=9)

# NOTE:  length(x) = 1 = length(x.1) in testList
\dontshow{stopifnot(}
all.equal(xy02, xy02.)
\dontshow{)}
##
## 4.  Subset one vector only 
##
xyz <- interpPairs(list(x=1:4), c(-1, 0, 1, 2))
# check 
xyz. <- list(x=2:3)
\dontshow{stopifnot(}
all.equal(xyz, xyz.)
\dontshow{)}
##
## 5.  Select an empty list (make sure this works)
##
x0 <- interpPairs(list(), 0:1)
# check 
x0. <- list()
names(x0.) <- character(0)
\dontshow{stopifnot(}
all.equal(x0, x0.)
\dontshow{)}

##
## 6. text
##
j.5 <- interpPairs(list(x.0='', x.1=c('a', 'bc', 'def')), 0.5)
# check  
j.5. <- list(x=c('a', 'bc', ''))
\dontshow{stopifnot(}
all.equal(j.5, j.5.)
\dontshow{)}

##
## 6. text, 1 argument 
##
j.50 <- interpPairs(list(x.1=c('a', 'bc', 'def')), 0.5)
# check  
\dontshow{stopifnot(}
all.equal(j.50, j.5.)
\dontshow{)}

##
## 7.  with elements of class call
##
xc <- interpPairs(list(x=1:3, y=quote(x+sin(pi*x/6))), 0:1)
# check
xc. <- list(x=1:3, y=1:3+sqrt(c(1, 3, 4)/4))
\dontshow{stopifnot(}
all.equal(xc, xc.)
\dontshow{)}

##
## 8.  A more complicated example with elements to eval
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
## 9.  using envir
##
  RlogoDiag <- list(x0=quote(Rlogo.$xleft), 
                  y0=quote(Rlogo.$ybottom), 
                  x1=quote(Rlogo.$xright), 
                  y1=quote(Rlogo.$ytop) ) 

  RlogoD <- interpPairs(RlogoDiag, .p=1, 
                      envir=list(Rlogo.=RlogoInterp) ) 
  RlogoD. <- RlogoInterp[c('xleft', 'ybottom', 'xright', 'ytop')]
  names(RlogoD.) <- c('x0', 'y0', 'x1', 'y1')
\dontshow{stopifnot(}
  all.equal(RlogoD, RlogoD.)
\dontshow{)}
}
##
## 10.  assign;  no interp but should work   
##
tstAsgn <- as.list(quote(op <- (1:3)^2))
intAsgn <- interpPairs(tstAsgn, 1)

# check 
intA. <- list(eval(`<-`), 
              quote(op), c(1, 4, 9))
names(intA.) <- c('X', 'X.3', 'X.2')
\dontshow{stopifnot(}
all.equal(intAsgn, intA.)
\dontshow{)}
}
\keyword{manip}
