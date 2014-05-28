animate1 <- function(plotObject, nFrames=NULL, iFrame=NULL,
                      endFrames=round(0.2*nFrames), ...){
  UseMethod('animate1')
}

animate1.function <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), plot.it=TRUE, ...){
##
## 1.  Set up
##
  plotName <- deparse(substitute(plotObject), 25)
  if(!is.function(plotObject)){
    stop('plotObject must be a function;  class(', 
         plotName, ') = ', class(plotObject))
  }
  po <- plotObject
  bo <- body(po)
  nbo <- length(bo)
  if(nbo<1){
    stop('length(body(plotObject = ', plotName, ')) = ', 
         nbo, ';  too short')
  }
#  2.1.  Check class(bo) and class(bo[[1]])
#        If they aren't "{" and "name",
#        either plotObject is  not a function
#        or I don't understand something critical in this.
  if(class(bo)[1] != "{"){
      stop("class(body(plotObject = ", plotName, 
           ")) is ", class(bo), ';  must be "{"')
  }        
  if(!is(bo[[1]], 'name')){
      stop("class(body(plotObject = ", plotName, 
          ")[[1]]) is ", class(bo[[1]]), ';  must be "name"')
  }
##
## 2.  iFrame & nFrames?
##
  nFr <- nFramesDefault(plotObject, nFrames, iFrame)
  nFrames <- as.numeric(nFr)
  if(is.null(iFrame))iFrame <- nFrames
##
## 3.  call interpPairs for all but the first element of bo  
##
  Bo <- bo
  ibo <- seq(2, length=nbo-1)
  for(ib in ibo){
    boi <- bo[[ib]]
    bi <- interpPairs(boi, nFrames=nFrames, 
        iFrame=iFrame, endFrames=endFrames, ...)
    Bo[[ib]] <- bi 
    if(plot.it)eval(bi)#, ...)     
  }
##
## 3.  done 
##
  body(po) <- Bo
  invisible(po)
}