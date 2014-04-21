animate <- function(plotList, nFrames=NULL, iFrames=NULL,
        filenames='%s%05d.png',
        endFrames=round(0.2*nFrames),
        framesFile='framesFiles.txt', duration,
        graphicsFunArgs=list(), ...){
##
## 1.  nFrames & iFrames?
##
#  1.1.  nFr <- nFrames
    nFr <- nFramesDefault(plotList, nFrames, iFrames)
    nFrames <- as.numeric(nFr)
    if(is.na(nFrames)){
        stop('nFrames not specified.')
    }
    if(is.null(iFrames)) iFrames <- 1:nFrames
#  1.2.  Fix any 'plot' element with tail(lastFrames, 1) = NA
    plot.lF.NA <- attr(nFr, 'plot.lastFrame.NA')
    for(jp in plot.lF.NA){
        n.lF <- is.na(plotList[[jp]]$lastFrame)
        plotList[[jp]]$lastFrame[n.lF] <- (nFrames-endFrames+1)
    }
##
## 2.  duration
##
    if(missing(duration)){
        if(missing(filenames)){
            duration <- 2
        } else duration <- 0.04
    }
##
## 3.  Create plots on the screen or write to a file?
##

    toFile <- !missing(filenames)
    if(toFile){
#  3.1.  Any NAs?
        Oops <- which(is.na(filenames[iFrames]))
        if(length(Oops)>0){
            stop('NA detected in filenames[iFrames] with number ',
                 Oops[1])
        }
#  3.2.  Check extensions
        graphicsFun <- c(bmp='bmp', jpg='jpeg', jpeg='jpeg',
                png='png', tif='tiff', tiff='tiff', svg='svg',
                ps='cairo_ps', pdf='cairo_pdf')
#        if(!require(raster)){
#            stop('Need function extension{raster};  not available.')
#        }
        ext <- Extension(filenames)
        gFns <- graphicsFun[ext]
        oops <- which(is.na(gFns))
        if(length(oops)>0){
            stop('Illegal extension on filenames[', oops[1],
                 '] = ', filenames[oops[1]],
                 ';\nLegal extensions = ', paste(names(graphicsFun),
                      collapse=', '))
        }
    }
##
## 4.  Create iFrames plots
##
    for(iFrame in iFrames){
        cat(iFrame, '')
        if(toFile){
#           Create empty file to hold the plot
            gFA <- graphicsFunArgs
            gFA$filename <- filenames[iFrame]
            do.call(gFns[iFrame], gFA)
        }
        animate1(plotList, nFrames, iFrame,
                 endFrames, ...)
        if(toFile){
            dev.off()
#           write to framesFile
            cat('file ',filenames[iFrame],
                '\nduration ', duration, '\n',
                file=framesFile, append=TRUE)
        } else {
            Sys.sleep(duration)
        }
#
        if((iFrame%%10)==0) cat('\n')
    }
}
