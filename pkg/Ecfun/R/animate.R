animate <- function(plotObject, nFrames=NULL, iFrames=NULL,
        filenames='%s%05d.png',
        endFrames=round(0.2*nFrames),
        framesFile='framesFiles.txt', duration, envir=list(), 
        pairs=c('1'='\\.0$', '2'='\\.1$', replacement=''),
        graphicsFun = c(bmp='bmp', jpg='jpeg', jpeg='jpeg',
            png='png', tif='tiff', tiff='tiff', svg='svg',
            ps='cairo_ps', pdf='cairo_pdf'),
        graphicsFunArgs=list(), enforceEndFrames=FALSE, ...){
##
## 1.  plotList <- plotObject or animate1(plotObject, ...) 
##
  if(is.function(plotObject)){
    plotList <- animate1.function(plotObject, nFrames=NULL, 
        iFrame=NULL, endFrames=round(0.2*nFrames), 
        plot.it=FALSE, ...)
  } else {
    plotList <- plotObject 
  }
##  
## 2.  nFrames & iFrames?
##
#  2.1.  nFr <- nFrames
  if(!missing(filenames)){
    nFiles <- length(filenames)
    if(is.null(nFrames)){
      nFrames <- nFiles 
    } else {
      if((nF <- length(nFrames)) !=1){
        stop('length(nFrames) = ', nF, ';  must be 1')
      }
      if(nFrames != nFiles){
        stop('nFrames = ', nFrames, 
             ' must equal length(filenames) = ', nFiles)
      }
    }
  }
  nFr <- nFramesDefault(plotList, nFrames, iFrames, 
                        envir=envir)
  nFrames <- as.numeric(nFr)
  if(is.na(nFrames)){
        stop('nFrames not specified.')
  }
  if(is.null(iFrames)) iFrames <- 1:nFrames
#  2.2.  Fix any 'plot' element with tail(lastFrames, 1) = NA
  plot.lF.NA <- attr(nFr, 'plot.lastFrame.NA')
  for(jp in plot.lF.NA){
      lF <- getElement2(plotList[[jp]], 'lastFrame', envir=envir)
      n.lF <- is.na(lF)
      lF[n.lF] <- (nFrames-endFrames+1)
      plotList[[jp]]$lastFrame <- lF 
    }
##
## 3.  duration
##
    if(missing(duration)){
        if(missing(filenames)){
            duration <- 2
        } else duration <- 0.04
    }
##
## 4.  Create plots on the screen or write to a file?
##
    toFile <- !missing(filenames)
    if(toFile){
#  4.1.  Any NAs?
        Oops <- which(is.na(filenames[iFrames]))
        if(length(Oops)>0){
            stop('NA detected in filenames[iFrames] with number ',
                 Oops[1])
        }
#  4.2.  Check extensions
#        if(!require(raster)){
#            stop('Need function extension{raster};  not available.')
#        }
        ext <- tools::file_ext(filenames)
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
## 5.  Create iFrames plots
##
    for(iFrame in iFrames){
        cat(iFrame, '')
        if(toFile){
#           Create empty file to hold the plot
            gFA <- graphicsFunArgs
            gFA$filename <- filenames[iFrame]
            do.call(gFns[iFrame], gFA)
        }
        animate1.list(plotList, nFrames, iFrame,
                 endFrames, envir, ...)
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
