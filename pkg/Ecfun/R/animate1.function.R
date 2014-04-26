animate1 <- function(plotObject, nFrames=NULL, iFrame=NULL,
                      endFrames=round(0.2*nFrames), ...){
  UseMethod('animate1')
}

animate1.function <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), plot.it=TRUE, ...){
##
## 1.  iFrame & nFrames
##
#  1.1.  Need to rewrite nFramesDefault to accept a function
#        or use simpler defaults
#    nFr <- nFramesDefault(plotObject, nFrames, iFrame)
#    nFrames <- as.numeric(nFr)
#  1.2.  simple defaults:  iFrame=nFrames=10
#  if(is.null(nFrames)){
#      if(is.null(iFrame)){
#          nFrames <- 10
#          iFrame <- nFrames
#      } else {
#          nFrames <- iFrame
#      }
#  } else {
#      if(is.null(iFrame)) iFrame <- nFrames
#  }
##
## 2.  Set up
##
  bo <- body(plotObject)
  nbo <- length(bo)
#  2.1.  Check class(bo) and class(bo[[1]])
#        If they aren't "{" and "name",
#        either plotObject is  not a function
#        or I don't understand something critical in this.
  if(class(bo) != "{"){
      stop("class(body(plotObject)) is ", class(bo),
           ';  must be "{"')
  }        
  if(class(bo[[1]]) != 'name'){
      stop("class(body(plotObject)[[1]]) is ", class(bo[[1]]),
           ';  must be "name"')
  }
##
## 3.  Convert to a list 
##
  plotList <- vector('list', nbo-1)
  for(iStep in seq(length=nbo-1)){
    iS1 <- iStep+1
    ibo <- bo[[iS1]]
    ib <- as.list(ibo)
    if(names(ib)[1]==''){
      names(ib)[1] <- 'fun'
    }
    plotList[[iStep]] <- ib
  }   
##
## 4.  call animate1.list
##
  if(plot.it){
    animate1.list(plotList, nFrames=nFrames, iFrame=iFrame,
                  endFrames=endFrames, ...)                  
  }  
  invisible(plotList)
}