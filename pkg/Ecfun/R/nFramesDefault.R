nFramesDefault <- function(plotList, nFrames=NULL, iFrames=NULL,
                     min.nFrames=c(nFrames=10, overMax=1.3)){
##
## 1.  If nFrames is supplied, prepare to return it.
##
    if(is.null(nFrames)){
        chkOnly <- FALSE
    } else {
        chkOnly <- TRUE
        if(length(nFrames) != 1){
            print(nFrames)
            stop('length(nFrames) != 1')
        }
        if(!is.numeric(nFrames)){
            print(nFrames)
            stop('nFrames is not numeric;  class = ',
                 class(nFrames))
        }
        if(nFrames<1){
            stop('nFrames = ', nFrames, ';  must be positive.')
        }
    }
##
## 2.  Compute plot.lastFrame
##
    Fns <- sapply(plotList, getElement2, name='fun', default=NA)
    plotj <- which(Fns=='plot')
    plot.lastFrame <- NA
    plot.lastj <- NA
    plot.lastF.NA <- rep(FALSE, length(plotj))
#    names(plot.lastF.NA) <- names(Fns)[plotj]
    for(j in seq(along=plotj)){
        jp <- plotj[j]
        ploj <- plotList[[jp]]
        lastF <- getElement2(ploj, 'lastFrame')
        lastF1 <- tail(lastF, 1)
        if(is.na(lastF1)){
            plot.lastF.NA[j] <- ('lastFrame' %in% names(ploj))
        } else {
#            plot.lastF.NA[j] <- FALSE
            if(!is.null(nFrames)){
                Keep <- getElement2(ploj, 'Keep')
                Keep. <- tail(Keep, 1)
#                if((!is.na(Keep)) && (!Keep)
#                   && (lastF1 < nFrames)){
#                    warning('tail(lastFrame, 1) = ', lastF1,
#                            ' < nFrames = ', nFrames,
#                            '\nwith fun="plot" and Keep=FALSE')
#                }
            }
            if(is.na(plot.lastFrame)){
                plot.lastFrame <- lastF1
                plot.lastj <- jp
            } else {
                if(lastF1>plot.lastFrame){
                    plot.lastFrame <- lastF1
                    plot.lastj <- jp
                }
            }
        }
#   If is.na(tail, lastF, 1)),
#   this is a signal to the software to replace tail(lastF, 1)
#   with some appropriate number.
    }
##
## 3.  Compute maxFrame
##
#  3.1.  First check iFrames
    if(is.null(iFrames)){
        maxFrame <- 0
    } else {
        iFr.oops <- which(is.na(iFrames))
        if(length(iFr.oops)>0){
            stop('NAs not allowed in iFrames; found in iFrames[',
                 iFr.oops[1], ']')
        }
        eri <- which(iFrames>plot.lastFrame)
#        if(length(eri)>0){
#            stop('iFrame[', eri[1], '] = ', iFrames[eri[1]],
#                 ' > tail(plotList$', names(Fns)[plot.lastj],
#                 '$lastFrame, 1) = ', plot.lastFrame)
#        }
        maxFrame <- max(iFrames)
    }
#   3.2.  Now loop over elements of plotList
    nFns <- length(Fns)
    max. <- function(...){
        dots <- list(...)
        nd <- length(dots)
        mx <- (-Inf)
        for(i in seq(length=nd)){
            if(is.null(dots[[i]])) next
            if(any(!is.na(dots[[i]]))){
                mx <- max(mx, dots[[i]], na.rm=TRUE)
            }
        }
        mx
    }
    for(jFn in seq(length=nFns)){
        plj <- plotList[[jFn]]
        firstFr <- getElement2(plj, 'firstFrame')
        err <- which(firstFr>plot.lastFrame)
        if(length(err)>0){
            stop('plotList$', names(Fns)[jFn], '$firstFrame[',
                 err[1], '] = ', firstFr[err],
                 ' > tail(plotList$', names(Fns)[plot.lastj],
                 '$lastFrame, 1) = ', plot.lastFrame)
        }
        lastFr <- getElement2(plj, 'lastFrame')
        erl <- which(lastFr>plot.lastFrame)
        if(length(erl)>0){
            stop('plotList$', names(Fns)[jFn], '$firstFrame[',
                 erl[1], '] = ', lastFr[erl],
                 ' > tail(plotList$', names(Fns)[plot.lastj],
                 '$lastFrame, 1) = ', plot.lastFrame)
        }
        maxFrame <- max.(maxFrame, firstFr, lastFr)
    }
##
## 4.  wrap up
##
    if(chkOnly){
        attr(nFrames, 'plot.lastFrame.NA') <- plotj[plot.lastF.NA]
        return(nFrames)
    }
    if(is.na(plot.lastFrame)){
        nFr <- max(min.nFrames[1], round(min.nFrames[2] * maxFrame))
        attr(nFr, 'plot.lastFrame.NA') <- plotj[plot.lastF.NA]
        return(nFr)
    }
    attr(plot.lastFrame, 'plot.lastFrame.NA') <- plotj[plot.lastF.NA]
    return(plot.lastFrame)
}
