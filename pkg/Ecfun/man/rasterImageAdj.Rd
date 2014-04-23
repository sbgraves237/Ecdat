\name{rasterImageAdj}
\alias{rasterImageAdj}
\title{
  rasterImage adjusting to zero distortion
}
\description{
  Call \code{rasterImage} to plot \code{image} from \code{(xleft,
    ybottom)} to either \code{xright} or \code{ytop}, shrinking one to
  avoid distortion.
}
\usage{
rasterImageAdj(image, xleft=par('usr')[1], ybottom=par('usr')[3],
     xright=par('usr')[2], ytop=par('usr')[4], angle = 0,
     interpolate = TRUE, ...)
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
    angle of rotation (in degrees, anti-clockwise from positive x-axis,
    about the bottom-left corner).
  }
  \item{interpolate}{
    a logical vector (or scalar) indicating whether to apply linear
    interpolation to the image when drawing.
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

  6.  Xr <- xleft+imageUnitsAdj[1];  Yt <- ybottom+imageUnitsAdj[2].

  7.  \code{rasterImage(image, xleft, ybottom, Xr, Yt, angle,
    interpolate, ...)}
}
\value{
  Graphical parameters set by \code{\link{rasterImage}}.
}
%\source{}
%\references{}
\author{Spencer Graves}
\seealso{
  \code{\link{rasterImage}}
}
\examples{
logo.jpg <- paste(R.home(), "doc", "html", "logo.jpg",
                  sep = .Platform$file.sep)
if(require(jpeg)){
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
}
}
\keyword{hplot}