\name{animate}
\alias{animate}
\alias{animate1}
\alias{animate1.list}
\alias{animate1.function}
\alias{nFramesDefault}
\title{
  Create a series of plots for an animation
}
\description{
  \code{animate} creates a series of plots.  If
  \code{missing(filenames)}, pause for \code{duration} seconds after the
  completion of one plot before starting the next.  Otherwise,
  \code{animate} produces a series of graphics files with names
  specified in filenames[iFrames].

  The primary output is intended to be a series of image files to be
  combined with an audio file using other software such as FFmpeg.

  \code{animate1} creates only one plot of the series.
}
\usage{
animate(plotObject, nFrames=NULL, iFrames=NULL,
        filenames='\%s\%05d.png',
        endFrames=round(0.2*nFrames),
        framesFile='framesFiles.txt', duration,
        graphicsFunArgs=list(), ...)
%\method{animate1}{default}(plotObject, nFrames=NULL, iFrame=NULL,
%        endFrames=round(0.2*nFrames), ...)
\method{animate1}{list}(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), ...)
\method{animate1}{function}(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), plot.it=TRUE, ...)        
nFramesDefault(plotObject, nFrames=NULL, iFrames=NULL,
         min.nFrames=c(nFrames=10, overMax=1.3))
}
\arguments{
  \item{plotObject}{
    Either a list or a function that can be coerced by 
    \code{animate1.function} to a list acceptable to \code{animate1.list}.  
    
    \code{animate} executes repeated calls to \code{animate1}, with each 
    call creating a potentially different plot.  
    
    If a list, it must be a list of lists that specify function calls 
    that may be modified depending on iFrame to create the different 
    plots.  
    
    The first element of the list may be a call to \code{\link{par}}.  
    An early element of the list must also contain informaiton needed to 
    create the basic plot.  Subsequent elements of the master list add 
    elements to the basic plot.  
    
    For \code{j} in \code{1:length(plotObject)}
    \code{plotObject[[j]]} starts with a 'fun' element containing the name
    of a function to call with arguments constructed from other elements
    of \code{plotObject[[j]]}.

    \code{plotObject[[j]]} can optionally contain elements
    \code{firstFrame}, \code{lastFrame}, and \code{Keep}.  The length of
    the default for these is code{lenFLK}, defined as follows:  If
    \code{plotObject[[j]]} has element(s) \code{x} or \code{y},
    \code{lenFLK} is the max length of these elements.  Otherwise,
    \code{lenFLK} = 1.

    The default for \code{firstFrame} is \code{seq(1,
    nFrames-endFrames+1, length=lenFLK)}.  The default for
    \code{lastFrame} is different:  It's \code{rep(nFrames-endFrames+1,
    lenFLK)}.  The default for \code{Keep} is \code{rep(TRUE, lenFLK)}.
    This is designed to produce the image of linear growth of points and
    lines in a standard plot.

    \code{plotObject[[j]][['fun']]} is not executed if \code{iFrame} <
    \code{firstFrame}.  For some functions, other elements of
    \code{plotObject[[j]]} are modified using linear interpolation with
    \code{firstFrame} <= \code{iFrame} <=  \code{lastFrame}, as
    described further below.  With \code{iFrame} > \code{lastFrame},
    \code{plotObject[[j]]['fun']} is executed when \code{iFrame} >
    \code{lastFrame} only if \code{Keep} = \code{TRUE}.  This allows
    different functions to be modified in different ways to produce a
    sequence of images simulating motion.

    Any \code{Keep} component of an element of \code{plotObject} with a
    component \code{fun} = 'plot' must end in TRUE.  If not and if
    \code{any( lastFrame < nFrames )}, the function
    \code{nFramesDefault} (called by \code{animate} and \code{animate1})
    generates warnings that plots may not appear for \code{iFrame} >
    \code{max(lastFrame)}.

    Apart from \code{firstFrame}, \code{lastFrame}, and \code{Keep}, the
    names of columns of \code{plotObject[[j]]} are names of arguments for
    \code{plotObject[[j]][['fun']]} with one more exception:  If a single
    argument is to be interpolated linearly, ".0" and ".1" may be
    appended;  in that case, the ".0" value is used with \code{iFrame} =
    \code{firstFrame}, and the ".1" value is used when \code{iFrame} =
    \code{lastFrame}.

    Elements of \code{plotObject[[j]]} can optionally be quoted
    expressions like \code{quote(x+fn(.6))} provided \code{x} and
    \code{fn} are either defined globally or appear earlier in
    \code{plotObject[[j]]}.  (For this purpose, \code{x} may be defined
    indirectly via \code{x.0} and \code{x.1}.)

    If \code{names(plotObject)[j]} == 'text', the columns are used to
    construct a call to \code{text}, using linear interpolation of the
    number of characters plotted for each frame.
  }
  \item{nFrames}{
    number of distinct plots to create.

    The default is computed by the function \code{nFramesDefault}.  If
    the \code{nFrames} argument to \code{nFramesdefault} is not NULL,
    then that number is used.

    \code{nFramesDefault} begins by computing \code{plot.lastFrame} from
    any element of \code{plotObject} with fun='plot'.  If such an element
    exists and has a component \code{lastFrame}, \code{plot.lastFrame}
    is \code{tail(lastFrame, 1)} for this 'plot' element of
    \code{plotObject}.  If no such 'plot' element exists or exists but
    does nothave a component \code{lastFrame}, then
    \code{plot.lastFrame} = \code{NA}.

    If \code{plot.lastFrame} is not \code{NA}, any element
    \code{plotObject} with components \code{firstFrame} or
    \code{lastFrame} exceeding \code{plot.lastFrame} generates an error
    message.  Barring an error, if argument \code{nFrames} is provided
    and exceeds \code{plot.lastFrame}, AND the \code{Keep} component
    associated with \code{plot.lastFrame} is \code{FALSE}, issue a
    warning.  Otherwise, if argument \code{nFrames} is \code{NULL}, the
    function \code{nFramesDefault} returns \code{plot.lastFrame} when it
    is not \code{NA}.

    If code{plot.lastFrame} is \code{NA}, \code{nFramesDefault} computes
    \code{maxFrame} as the max of \code{iFrames} and non-NAs in
    \code{firstFrame} and \code{lastFrame} of any element of
    \code{plotObject}.

    \code{nFramesDefault} returns \code{nFrames} if it is a number
    greater than 1 and \code{max(min.nFrames[1], min.nFrames[2] * maxFrame)}
    otherwise.
  }
  \item{iFrames}{
    integer vector giving the indices of frames to create.  1 <= iFrames
    <= nFrames.  Defaults to 1:nFrames.
  }
  \item{filenames}{
    Names of image files to create.  Ignored if \code{missing} and
    \code{pause} > 0.

    If \code{length(filenames)} == 1 and \code{length(iFrames)} > 1,
    \code{filenames} <- \code{sprintf(filenames,
      deparse(substitute(plotCalls)), iFrames)}.

    Otherwise, if(length(filenames) != length(iFrames)), filenames <-
    filenames[iFrames].

    NOTE:  All filenames are assumed to have the same extension, and the
    function called to create the graphics object is obtained via
    graphicsFun = c(bmp='bmp', jpg='jpeg', png='png', tif='tiff',
    svg='svg', ps='cairo_ps', pdf='cairo_pdf').
  }
  \item{endFrames}{
    Number of frames to hold constant at the end.
  }
  \item{framesFile}{
    Name of a file to which to write (append) the list of files
    created for subsequent processing by FFmpeg.

    For each image file written, two records are appended to
    \code{framesFile}:

    (1) cat('file', filenames[iFrame])

    (2) cat('duration', duration)
  }
  \item{duration}{
    seconds per frame.  If \code{filenames} is provided, this is the
    number written to \code{framesFile} as "duration";  see the
    discussion of \code{filenames} above.  In this case, \code{duration}
    defaults to 0.04 for 25 frames per second.

    If \code{filenemes} is not provided, this is the number of seconds
    to pause after the completion of each plot before starting the
    next.  In this case, \code{duration} defaults to 2 seconds.
  }
  \item{graphicsFunArgs}{
    argument list for use in creating output graphic files.  Example:
    If filenames uses an extension "png", then an output graphics file
    would be opened with \code{do.call('png', graphicsFunArgs)}.  This
    uses the \code{graphicsFun} object described with the
    \code{filenames} argument above to translate the file extension into
    the name of a graphics device.
  }
  \item{\dots}{
    optional arguments for plot functions
  }
  \item{iFrame}{
    integer giving the index of the single frame to create.  Default =
    nFrames.
  }
  \item{plot.it}{ 
    logical:  If \code{TRUE}, create the desired plot.  Otherwise, convert 
    \code{plotObject} to a \code{list}.  
  }
  \item{min.nFrames}{
    Two numbers giving default values for \code{nFramesDefault}:
    \code{min.nFrames[1]} gives the minimum default value for
    \code{nFramesDefault}.  \code{min.nFrames[2]} is a factor to
    multiply by the \code{maxFrame} value computed from
    \code{firstFrame} and \code{lastFrame} components of
    \code{plotObject}, as noted with the discussion of \code{nFrames}
    above.
  }
}
\details{
  1.  Call \code{nFramesDefault} to check consistency between arguments
    \code{nFrames}, \code{iFrames} and \code{plotObject};  compute
    defaults if \code{nFrames} or \code{iFrames} is \code{NULL}.

  2.  Set op <- par(mar=mar);  on.exit(par(op)).

  3.  for(iFrame in iFrames):

  3.1.  If \code{filenames} are provided (not the default), {
    graphicsFun <- c(bmp='bmp', jpg='jpeg', png='png', tif='tiff',
    svg='svg', ps='cairo_ps', pdf='cairo_pdf');
    do.call(graphicsFun[outFn], graphicsFunArgs)}.

  3.2.  Create an empty plot with limits specified in xylims as outlined
  in the discussion of that argument.

  3.3.  funs <- names(plotObject); for(fn in funs):

  3.3.1. ploti <- plotObject[[fn]]

  3.3.2.  plotj <- ploti adjusted per iFrame.

  3.3.3.  for each row in plotj, execute fn with arguments as adjusted
  in that row of the of plotj.

  3.4.  if(is.null(outFun)) Sys.sleep(pause) else dev.off().

  4.  done.
}
%\source{}
\value{
  \code{animate} and \code{animate1} return "done" invisibly.

  \code{nFramesDefault} returns an integer or \code{NA} with an attribute
  \code{plot.lastFrame.NA} identifying any elements of \code{plotObject}
  with components \code{fun} = 'plot' and \code{lastFrame} ending in
  \code{NA}.  If no element of \code{plotObject} meets these criteria, the
  attribute \code{plot.lastFrame.NA} = \code{numeric(0)}.
}
%\references{}
\seealso{
  \code{\link{png}}, \code{\link{Sys.sleep}},
  \code{\link{body}}, \code{\link{rasterImageAdj}}
}
\examples{
##
## 1.  Number of CRAN packages vs. time
##
logo.jpg <- paste(R.home(), "doc", "html", "logo.jpg",
                  sep = .Platform$file.sep)
if(require(jpeg) && require(Ecdat)){
  Rlogo <- readJPEG(logo.jpg)

#  data(Ecdat::CRANpackages)
  CRANobs <- nrow(CRANpackages)

  CRANgrowth <- list(list(fun='plot',
                    x=CRANpackages[['Date']],
                    y=CRANpackages[['Packages']],
                    xlim=range(CRANpackages[['Date']]),
                    ylim=range(CRANpackages[['Packages']]),
                    log='y'),
             list(fun='rasterImageAdj', image=Rlogo,
                  xleft=as.Date('2002-01-01'), ybottom=1000,
                  xright=as.Date('2005-01-01'), ytop=5000) )
  animate(CRANgrowth)

##
## 2.  Map zoom out from Aukland, NZ, to the entire world
##     in 23 frames with arrows and a rotating R logo.
##

# 1.  Aukland, NZ
# 2.  traveling to Switzerland, frames 1-11
# 3-4.  traveling NZ to US, China,
#       frames 11-21
# 5-6.  traveling CH to Russia, UK, frames 11-21
  RlogoLocations <- list(
    fun='rasterImageAdj',
    firstFrame=c( 1, 1, rep(11, 4)),
    lastFrame =c(10,10, rep(21, 4)),
    Keep = TRUE,
    image=Rlogo,
    xleft.0 = c(NZ=176.5,CH=172,US=171,  CN=177,RU= 9.5,UK= 8),
    xleft.1 = c(NZ=176.5,CH=  9,US=-73.5,CN=125,RU= 37, UK= 2),
    ybottom.0=c(NZ=-37,  CH=-34,US=-34,  CN=-33,RU= 48, UK=47),
    ybottom.1=c(NZ=-37,  CH= 47,US= 46,  CN= 32,RU=55.6,UK=55),
    xright=quote(xleft+xinch(0.6)),
    ytop = quote(ybottom+yinch(0.6)),
    angle.0 =0,
    angle.1 =c(NZ=0,CH=3*360,US=5*360, CN=2*360,RU=360,UK=360)
    )

  Rarrows <- list(
    fun='Arrows',
    firstFrame=c( 1, rep(11, 4)),
    lastFrame =c(10, rep(21, 4)),
    Keep = TRUE,
    x0 = c(CH=171,  US=170,CN=175,  RU=  9, UK=7.5),
    x1.0=c(CH=171,  US=170,CN=175,  RU=  9, UK=7.5),
    x1.1=c(CH=  8.5,US=-74,CN=123,  RU= 36, UK=1),
    y0 = c(CH=-34.5,US=-35,CN=-32.5,RU= 48, UK=47),
    y1.0=c(CH=-34.5,US=-35,CN=-32.5,RU= 48, UK=47),
    y1.1=c(CH= 46,  US= 39,CN= 31.5,RU=55.6,UK=55),
    length.0=0, length.1=.4, lwd=3, col='red')

  RFrames <- NA
  RF.5 <- 10
# xlim and ylim will both have 2 elements for all iFrame,
# because firstFrame and lastFrame together specify
# nonoverlapping ranges that are duplicated:
# (1 to RF.5) and (RF.5+1 to RFrames),
# AND Keep=FALSE.
# (xlim.0 and ylim.0) are used at the beginning of each interval
# (iFrame = 1 and RF.5+1), and
# (xlim.1 and ylim.1) are used at the end
# (iFrame = RF.5 and RFrames).
  RhistPlot <- list(fun='plot',
        firstFrame=rep(c(1, RF.5+1),e=2),
        lastFrame=rep(c(RF.5, RFrames), e=2),
        Keep=rep(c(FALSE, TRUE), e=2),
        xlim.0=c(165, 179,   6,179),
        xlim.1=c(  6, 179,-180,179),
        ylim=c(-48, -34, -48, 48),
        x=c(179, 165,   6,179),
        y=c(-48, -34, -48, 48),
	type='n', bty='n', axes=FALSE, asp=1,
        xlab='', ylab='')
  RhistTitle <- list(fun='title',
        main='R History',
        sub='Wikipedia, "R (programming language)"',
        line=-3)

  Rhist <- list(
    setup0=list(fun='par', mar=rep(0.1, 4)),
    setup=RhistPlot,
    title=RhistTitle,
    plotMap=list(fun='map', add=TRUE),
    Rlogo.=RlogoLocations,
    Arrows.=Rarrows)

# check nFramesDefault:  default = round(1.3 * max(lastFrame)) = 27
  nFrames.Rhist <- nFramesDefault(Rhist)
# answer
  nFrames.Rh <- 27
  plot.lastFNA <- 2
  names(plot.lastFNA) <- 'setup'
  attr(nFrames.Rh, 'plot.lastFrame.NA') <- plot.lastFNA

\dontshow{stopifnot(}
all.equal(nFrames.Rhist, nFrames.Rh)
\dontshow{)}

if(require(maps)){
  animate(Rhist)
# Plot NZ in frame 1,
# Zoom to include Switzerland by frame 11 then the world by frame 21.
# Constant from frames 21 to the end.
# pause = 0.1 implies 10 frames per second, ignoring plot .


# Create 27 image files for a video:
  RhistFiles <- sprintf('Rhist\%05d.png', 1:27)
  animate(Rhist, filenames=RhistFiles)

# creates files Rhistory00001.png, Rhistory00002.png, ...,
# plus framesFiles.txt to be used by ffmpeg:
# ffmpeg -y -f concat -i framesFiles.txt -vf fps=2 -pix_fmt yuv420p Rhist.mp4
}
}
##
## 3.  Check nFramesDefault
##
chk0 <- nFramesDefault(list()) # 10
chk1 <- nFramesDefault(list(), 1) # 1
chk2 <- nFramesDefault(list(), iFrames=1:2) # round(1.3*2) = 3, rtn 10
chk9 <- nFramesDefault(list(), iFrames=1:9) # round(1.3*9) = 12
chkPlot0 <- nFramesDefault(list(plot=list(fun='plot'))) # 10
chkPlot4 <- nFramesDefault(list(plot=list(fun='plot', lastFrame=c(1, 4)),
                          other=list(firstFrame=1, lastFrame=2)),
                      iFrames=3)
# chkPlot4 = 4

# check
chk0. <- 10
attr(chk0., 'plot.lastFrame.NA') <- numeric(0)
\dontshow{stopifnot(}
all.equal(chk0, chk0.)
\dontshow{)}

chk1. <- 1
attr(chk1., 'plot.lastFrame.NA') <- numeric(0)
\dontshow{stopifnot(}
all.equal(chk1, chk1.)
\dontshow{)}

chk2. <- 10
attr(chk2., 'plot.lastFrame.NA') <- numeric(0)
\dontshow{stopifnot(}
all.equal(chk2, chk2.)
\dontshow{)}

chk9. <- 12
attr(chk9., 'plot.lastFrame.NA') <- numeric(0)
\dontshow{stopifnot(}
all.equal(chk9, chk9.)
\dontshow{)}

chkPlot0. <- 10
plot.lF.NA <- integer(0)
names(plot.lF.NA) <- character(0)
attr(chkPlot0., 'plot.lastFrame.NA') <- plot.lF.NA
\dontshow{stopifnot(}
all.equal(chkPlot0, chkPlot0.)
\dontshow{)}

chkPlot4. <- 4
attr(chkPlot4., 'plot.lastFrame.NA') <- plot.lF.NA
\dontshow{stopifnot(}
all.equal(chkPlot4, chkPlot4.)
\dontshow{)}
}
\keyword{hplot}