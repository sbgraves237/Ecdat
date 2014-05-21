animate1.list <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), envir=list(), 
        pairs=c('1'='\\.0$', '2'='\\.1$', replace0='', 
                        replace1='.2', replace2='.3'),
        enforceEndFrames=FALSE, ...){
##
## 1.  iFrame & nFrames?
##
  Envir <- envir 
  nFr <- nFramesDefault(plotObject, nFrames, iFrame, 
                          envir=rev(Envir))
  nFrames <- as.numeric(nFr)
  if(is.null(iFrame))iFrame <- nFrames
##
## 2.  set up
##
  Fns <- sapply(plotObject, getElement2, '', 
            name='fun', default=NA, envir=rev(Envir))
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
      Par <- interpPairs(plotObject[[jPar]], 1, 
                  envir=rev(Envir), Source='par', ...) 
      Envir[[names(plotObject)[jPar]]] <- Par 
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
# nameL <- checkNames(plotL, ...) 
# if pblms with the following, replace by checkNames 
  nameL <- names(plotL)
  if(is.null(nameL)){
    stop('plotObject list must have names;  does not.')
  }
  pL0 <- which(nchar(nameL)<1)
  if(length(pL0)>0){
    stop('plotObject list must have names; # ', pL0[1], 
         ' is blank.')
  }
  pLna <- which(is.na(nameL))
  if(length(pLna)>0){
    stop('plotObject list must have names; # ', pLna[1], 
         ' is NA.')
  }
  for(j in seq(length=nFns)){ 
#    if(j>14)browser()
    plotj <- plotL[[j]]
    firstF0 <- getElement2(plotj, 'firstFrame', 1, 
                      envir=rev(Envir))
    if(iFrame<min(firstF0)){
      Envir[[nameL[j]]] <- plotj       
      next 
    }
    x <- getElement2(plotj, 'x', envir=rev(Envir))
    y <- getElement2(plotj, 'y', envir=rev(Envir))
    lenFLK <- max(1, length(x), length(y))
    lastF1 <- (nFrames-endFrames+1)
    firstF <- getElement2(plotj, 'firstFrame',
                          seq(1, lastF1, length=lenFLK), 
                          envir=rev(Envir))
    if(enforceEndFrames){ 
      oops1 <- which(firstF>lastF1)
      if(length(oops1)>0){
          nFirstLast <- max(length(lastF1), length(firstF))
          lastF1. <- rep(lastF1, length=nFirstLast)
          firstF. <- rep(firstF, length=nFirstLast)
          warning('plotObject$', nameL[j], '$firstFrame[',
                  oops1[1], '] = ', firstF[oops1[1]],
                  ' > lastF = (nFrames-endFrames+1) = ',
                  lastF1.[oops1[1]],
                  ';\n  reducing firstFrame to lastF.' )
          firstF <- pmin(firstF., lastF1.)
      }
    }
    lastF <- getElement2(plotj, 'lastFrame',
                         rep(lastF1, lenFLK), 
                         envir=rev(Envir))
    if(enforceEndFrames){
        oopsL <- which(lastF>lastF1)
        if(length(oopsL)>0){
          nL2 <- max(length(lastF1), length(lastF))
          lastF2 <- rep(lastF1, length=nL2)
          lastF. <- rep(lastF, length=nL2)
          warning('plotObject$', nameL[j], '$lastFrame[',
                  oopsL[1], '] = ', lastF[oopsL[1]],
                  ' > lastF = (nFrames-endFrames+1) =  ',
                  lastF2[oopsL[1]],
                  ';\n reducing lastFrame to lastF.')
          lastF <- pmin(lastF2, lastF.)
        }
    }
    Kp <- getElement2(plotj, 'Keep', rep(TRUE, lenFLK), 
                      envir=rev(Envir))
##
## 5.  How far in the process?
##
    dF <- (lastF-firstF)
    pDone <- pmin((iFrame-firstF) / dF, 1)
    pDone[is.na(pDone)] <- 1
    pDone[(iFrame>lastF) & !Kp] <- (-1)
#      if(max(pDone)<0)next
#
    ploj <- interpPairs(plotj, pDone, rev(Envir), pairs, 
                        Source=nameL[j], ...)
    Envir[[nameL[j]]] <- ploj 
#
    if(max(pDone)<0)next
#      
    nKeep <- sum(pDone>=0)
##
## 6.  Fn.[j] == 'plot'?
##
    if(Fn.[j]=='plot'){
        if(!('xlab' %in% names(ploj))){
            ploj$xlab <- 'x'
        }
        if(!('ylab' %in% names(ploj))){
            ploj$ylab <- 'y'
        }
    }
##
## 7.  do.call
##
    ploj$fun <- NULL
    ploj$firstFrame <- NULL
    ploj$lastFrame <- NULL
    ploj$Keep <- NULL
    ploj$.proportion <- NULL 
#    browser()
#    do.call(Fn.[j], ploj)
    if(Fn.[j]=='<-'){    
#      dcj <- do.call(`<-`, ploj, 
#              envir=as.environment(Envir))
      dcj <- do.call(`<-`, ploj)
      Envir[[as.character(ploj[[1]])]] <- dcj
    } else {
#      do.call(Fn.[j], ploj, 
#              envir=as.environment(Envir))
      do.call(Fn.[j], ploj)
    }
  }
##
## 8.  Done
##
    invisible(Envir)
}