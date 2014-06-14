\name{rasterImageAdj}
\alias{rasterImageAdj}
\title{
  rasterImage adjusting to zero distortion
}
\description{
  Call \code{rasterImage} to plot \code{image} from \code{(xleft,
  ybottom)} to either \code{xright} or \code{ytop}, shrinking one 
  toward the center to avoid distortion.  
  
  \code{angle} specifies a rotation around the midpoint 
  (\code{(xleft+xright)/2}, \code{(ybottom+ytop)/2}).  This is 
  different from \code{\link{rasterImage}}, which rotates 
  around \code{(xleft, ybottom)}.  
  
  NOTE:  The code may change in the future.  The visual 
  image with rotation looks a little off in the examples 
  below, but the code seems correct.  If you find an example
  where this is obviously off, please report to the 
  maintainer -- especially if you find a fix for this.  
}
\usage{
rasterImageAdj(image, xleft=par('usr')[1], ybottom=par('usr')[3],
     xright=par('usr')[2], ytop=par('usr')[4], angle = 0,
     interpolate = TRUE, xsub=NULL, ysub=NULL, ...)
}
\arguments{
  \item{image}{
    a \code{raster} object, or an object that can be coerced to one by
    \code{\link{as.raster}}.
  }
  \item{xleft}{
    a vector (or scalar) of left x positions.
  }
  \item{ybottom}{
    a vector (or scalar) of bottom y positions.
  }
  \item{xright}{
    a vector (or scalar) of right x positions.
  }
  \item{ytop}{
    a vector (or scalar) of top y positions.
  }
  \item{angle}{
    angle of rotation in degrees, anti-clockwise about the centroid
    of \code{image}.  
    
    NOTE:  \code{\link{rasterImage}} rotates around 
    (\code{xleft}, \code{ybottom}).  \code{rasterImage} 
    rotates around the center \code{((xleft+xright)/2, 
    (ybottom+ytop)/2)}.  See the examples.  
  }
  \item{interpolate}{
    a logical vector (or scalar) indicating whether to apply linear
    interpolation to the image when drawing.
  }
  \item{xsub, ysub}{
    subscripts to subset \code{image}  
  }
  \item{\dots}{
    graphical parameters (see \code{\link{par}}).
  }
}
\details{
  1.  imagePixels = number of (x, y) pixels in \code{image}.  Do this
  using dim(as.raster(image))[2:1], because the first dimension of
  \code{image} can be either x or y depending on \code{class(image)}.
  For example \code{link[EBImage]{Image}} returns \code{dim} with x
  first then y and an optional third dimension for color.  A simple
  3-dimensional array is assumed by \code{\link{rasterImage}} to have
  the y dimension first.  \code{\link{as.raster}} puts all these in a
  standard format with y first, then x.

  2. imageUnits <- c(x=xright-xleft, ytop-ybottom)

  3.  xyinches = (x, y) units per inch in the current plot, obtained
  from \code{\link{xyinch}}.

  4.  Compute pixel density (pixels per inch) in both x and y
  dimension:  pixelsPerInch <- imagePixels * xyinches / imageUnits.

  5.  Compute imageUnitsAdj solving 4 for imageUnits and replacing
  pixelsPerInch by the max pixel density:  imageUnitsAdj <- imagePixels
  * xyinches / max(pixelsPerInch).

  6.  (dX, dY) = imageUnitsAdj/2 = half of the (width, height) 
  in plotting units.  
  
  7.  cntr = (xleft, ybottom) + (dX, dY).  
  
  xleft0  = cntr[1]+sin((angle-90)*pi/180)*dX*sqrt(2); 
  ybottom0= cntr[2]-cos((angle-90)*pi/180)*dY*sqrt(2);
  
  (xright0, ytop0) 
  = (upper right without rotation about lower left)
  xright0 = xleft0+imageUnitsAdj[2] 
  ytop0 = ybottom0+imageUnitsAdj[2]

  8.  \code{rasterImage(image, xleft0, ybottom0, 
            xright0, ytop0, angle, interpolate, ...)}
}
\value{
  a named vector giving the values of \code{xleft}, 
  \code{ybottom}, \code{xright}, and \code{ytop} 
  passed to \code{\link{rasterImage}}.  
  (\code{\link{rasterImage}} returns \code{NULL}, 
  at least for some inputs.)  This shows the adjustment, 
  shrinking toward the center and rotating as desired.  
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{rasterImage}}
}
\examples{
# something to plot 
logo.jpg <- paste(R.home(), "doc", "html", "logo.jpg",
                  sep = .Platform$file.sep)
if(require(jpeg)){
##
## 1.  Shrink as required
##
  Rlogo <- readJPEG(logo.jpg)
  \dontshow{stopifnot(}
  all.equal(dim(Rlogo), c(76, 100, 3))
  \dontshow{)}

  plot(1:2)
# default
  rasterImageAdj(Rlogo)

  plot(1:2, type='n', asp=0.75)
# Tall and thin
  rasterImage(Rlogo, 1, 1, 1.2, 2)
# Fix
  rasterImageAdj(Rlogo, 1.2, 1, 1.4, 2)

# short and wide
  rasterImage(Rlogo, 1.4, 1, 2, 1.2)
# Fix
  rasterImage(Rlogo, 1.4, 1.2, 2, 1.4)
##
## 2.  rotate 
##
#  2.1.  angle=90:  rasterImage left of rasterImageAdj
  plot(0:1, 0:1, type='n', asp=1)
  rasterImageAdj(Rlogo, .5, .5, 1, 1, 90)
  rasterImage(Rlogo, .5, .5, 1, 1, 90)
#  2.2.  angle=180:  rasterImage left and below 
  plot(0:1, 0:1, type='n', asp=1)
  rasterImageAdj(Rlogo, .5, .5, 1, 1, 180)
  rasterImage(Rlogo, .5, .5, 1, 1, 180)
#  2.3.  angle=270:  rasterImage below 
  plot(0:1, 0:1, type='n', asp=1)
  rasterImageAdj(Rlogo, .5, .5, 1, 1, 270)
  rasterImage(Rlogo, .5, .5, 1, 1, 270)
## 
## 3.  subset 
##
dim(Rlogo)
# 76 100 3
Rraster <- as.raster(Rlogo)
dim(Rraster)
# 76 100:  
# x=1:100, left to right 
# y=1:76, top to bottom 
rasterImageAdj(Rlogo, 0, 0, .5, .5, xsub=40:94)
}
}
\keyword{hplot}