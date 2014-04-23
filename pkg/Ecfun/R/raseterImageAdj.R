rasterImageAdj <- function(image, xleft=par('usr')[1],
     ybottom=par('usr')[3], xright=par('usr')[2],
     ytop=par('usr')[4], angle = 0, interpolate = TRUE,
                           ...){
##
## 1.  x, y pixels in image
##
#   dim(image) = y, x, RGB
    imagePixels <- dim(as.raster(image))[2:1]
#    imageAsp <- imagePixels[2]/imagePixels[1]
    names(imagePixels) <- c('x', 'y')
##
## 2.  x, y pixels per inch
##
#  2.1.  x, y units in specified region
    imageUnits <- c(x=xright-xleft, y=ytop-ybottom)
    if(par('xlog'))imageUnits[1] <- log10(xright/xleft)
    if(par('ylog'))imageUnits[2] <- log10(ytop/ybottom)
#  2.2.   plot units per inch
    xyinches <- xyinch(warn.log=FALSE)
#    plotAsp <- xyinches[2]/xyinches[1]
    names(xyinches) <- c('x', 'y')
#  2.3.  x, y pixels per inch in image region
    pixelsPerInch <- (imagePixels*xyinches/imageUnits)
##
## 3.  Shrink imageUnits to max(PixelsPerInch)
##
    imageUnitsAdj <- (imagePixels*xyinches/max(pixelsPerInch))
##
## 4.  Adjust xright, ytop
##
    if(par('xlog')){
        Xr <- (xleft*10^imageUnitsAdj[1])
    } else Xr <- xleft+imageUnitsAdj[1]
#
    if(par('ylog')){
        Yt <- ybottom*10^imageUnitsAdj[2]
    } else Yt <- ybottom+imageUnitsAdj[2]
##
## 5.  rasterImage
##
    rasterImage(image=image, xleft=xleft, ybottom=ybottom,
                xright=Xr, ytop=Yt, angle=angle,
                interpolate=interpolate, ...)
}

