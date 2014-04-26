animate1.list <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), ...){
##
## 1.  iFrame & nFrames?
##
    nFr <- nFramesDefault(plotObject, nFrames, iFrame)
    nFrames <- as.numeric(nFr)
    if(is.null(iFrame))iFrame <- nFrames
##
## 2.  set up
##
    Fns <- sapply(plotObject, getElement2, name='fun', default=NA)
#    Fns <- sapply(plotObject, function(z){
#        fn <- z$fun
#        if(is.null(fn)){
#            NA
#        } else fn
#    })
    noFun <- which(is.na(Fns))
    if(length(noFun)>0){
        stop('plotObject[', noFun[1], '] must have an element ',
             '"fun";  it does not.')
    }
##
## 3.  par
##
    jPar <- which(Fns %in% 'par')
    if(length(jPar)>1){
        stop('Only one call to par allowed;  found ',
             length(jPar))
    }
    if(length(jPar)>0){
        Par <- plotObject[[jPar]]
        Par$fun <- NULL
        op <- do.call(par, Par)
        on.exit(par(op))
        plotL <- plotObject[-jPar]
        Fn. <- Fns[-jPar]
    } else {
        plotL <- plotObject
        Fn. <- Fns
    }
##
## 4.  loop over all function calls in plotObject
##
    nFns <- length(Fn.)
    for(j in seq(length=nFns)){
        plotj <- plotL[[j]]
        plotj$fun <- NULL
        lenFLK <- max(1, length(plotj$x), length(plotj$y))
        lastF1 <- (nFrames-endFrames+1)
        firstF <- getElement2(plotj, 'firstFrame',
                              seq(1, lastF1, length=lenFLK))
        oops1 <- which(firstF>lastF1)
        if(length(oops1)>0){
            nFirstLast <- max(length(lastF1), length(firstF))
            lastF1. <- rep(lastF1, length=nFirstLast)
            firstF. <- rep(firstF, length=nFirstLast)
            warning('plotObject$', names(Fn.)[j], '$firstFrame[',
                    oops1[1], '] = ', firstF[oops1[1]],
                    ' > lastF = (nFrames-endFrames+1) = ',
                    lastF1.[oops1[1]],
                    ';\n  reducing firstFrame to lastF.' )
            firstF <- pmin(firstF., lastF1.)
        }
        lastF <- getElement2(plotj, 'lastFrame',
                             rep(lastF1, lenFLK))
        oopsL <- which(lastF>lastF1)
        if(length(oopsL)>0){
            nL2 <- max(length(lastF1), length(lastF))
            lastF2 <- rep(lastF1, length=nL2)
            lastF. <- rep(lastF, length=nL2)
            warning('plotObject$', names(Fn.)[j], '$lastFrame[',
                    oopsL[1], '] = ', lastF[oopsL[1]],
                    ' > lastF = (nFrames-endFrames+1) =  ',
                    lastF2[oopsL[1]],
                    ';\n reducing lastFrame to lastF.')
            lastF <- pmin(lastF2, lastF.)
        }
        Kp <- getElement2(plotj, 'Keep', rep(TRUE, lenFLK))
        plotj$firstFrame <- NULL
        plotj$lastFrame <- NULL
        plotj$Keep <- NULL
##
## 5.  How far in the process?
##
        pDone <- pmin((iFrame-firstF) / (lastF-firstF), 1)
        pDone[is.na(pDone)] <- 1
        pDone[(iFrame>lastF) & !Kp] <- (-1)
        if(max(pDone)<0) next
        ploj <- interpPairs(plotj, pDone)
        nKeep <- sum(pDone>=0)
##
## 6.  text
##
        if(Fn.[j] == 'text'){
            lblj <- ploj$labels
            if(length(lblj) == nKeep){
                lbls <- plotj$labels
                nCh <- nchar(lbls)
                cumCh <- cumsum(nCh)
                cumP <- cumCh/tail(cumCh, 1)
                pCh <- nCh/tail(cumCh, 1)
                p.ch <- pmin((pDone-cumP)/pCh, 1)
                k.ch <- round(p.ch*nCh)
                lbls[k.ch>0] <- substring(lbls[k.ch>0], 1,
                                          k.ch[k.ch>0])
                lbls. <- lbls[pDone>=0]
                ploj$labels <- lbls.
                do.call(text, ploj)
                next
            } else {
                if(length(lblj)>1) {
                    warning('length of labels in call to text ',
                            'do not match other arguments;\n ',
                            '  ignoring the problem.')
                }
                do.call(text, ploj)
            }
        }
##
## 7.  Fn.[j] == 'plot'?
##
        if(Fn.[j]=='plot'){
#            if('xlim' %in% names(ploj)){
#                ploj$xlim <- range(ploj$xlim)
#            }
#            if('ylim' %in% names(ploj)){
#                ploj$ylim <- range(ploj$ylim)
#            }
            if(!('xlab' %in% names(ploj))){
                ploj$xlab <- 'x'
            }
            if(!('ylab' %in% names(ploj))){
                ploj$ylab <- 'y'
            }
        }
##
## 8.  do.call
##
        do.call(Fn.[j], ploj)
    }
##
## 9.  Done
##
    invisible('done')
}

