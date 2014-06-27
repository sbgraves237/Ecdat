animate1 <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), ...){
  UseMethod('animate1')
}

animate1.function <- function(plotObject, nFrames=NULL, iFrame=NULL,
        endFrames=round(0.2*nFrames), envir=new.env(), 
        plot.it=TRUE, ...){
##
## 1.  Set up
##
  plotName <- deparse(substitute(plotObject), 25)
  if(!is.function(plotObject)){
    stop('plotObject must be a function;  class(', 
         plotName, ') = ', class(plotObject))
  }
  plotList <- as.list(plotObject)
# The first elements of plotObject are the arguments
# The last is {...} obtained by body(po)
  po <- plotObject
  bo <- body(plotObject)
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
  if(is.environment(envir)){
    Envir <- envir 
  } else {
    Envir <- new.env()
    for(X in names(envir)){
      Envir[[X]] <- eval(envir[[X]])
    }
  }
# NOTE:  if(is(Envir, 'environment')), 
# Envir is an alias for envir, 
# so changing Envir changes envvir, 
# contrary to lists.  
  for(X in head(names(plotList), -1)){
    Envir[[X]] <- plotList[[X]]
  }
  for(ib in ibo){
    boi <- bo[[ib]]
    bi <- interpPairs(boi, nFrames=nFrames, 
        iFrame=iFrame, endFrames=endFrames, 
        envir=Envir, ...)
    if(is.null(bi)){
      bi <- enquote(NULL)
    }# else {
#      if(names(bi)[1]==''){
#        bi <- pryr::standardise_call(bi)
#        for(i in seq(2, length=max(0, length(bi)-1))){
#          bi[[i]] <- eval(bi[[i]], Envir)
#        }
#      }
#    }
    Bo[[ib]] <- bi
    if(bi[[1]]=='<-'){
#      assign(as.character(bi[[2]]), eval(bi[[3]]) )  
      eval(bi)
    } else {
      if(plot.it){
        if(bi[[1]]=='on.exit'){
          do.call(on.exit, as.list(bi[-1]))
        } else {
          do.call(as.character(bi[[1]]), as.list(bi[-1]), 
                  envir=Envir)
#          eval(bi, Envir)
        }
      }
    }
#    Envir[[ib]] <- bi 
#    if(plot.it){
#      eval(bi)#, ...)           
#      eval(bi, Envir)      
# PROBLEM:  This does not work properly for "assign" like 
# op <- par(mar=rep(.1, 4))
# *** I don't know where op is ... 
# a temp environment discarded after "eval" is done?  
#    }
  }
##
## 4.  done 
##
#  *** I don't know how to make the local 
#  environment available to po 
#  It's environment is .GlobalEnv, 
#  inherited from plotObject :-(
#  Envir$iFrame <- iFrame
#  Envir$nFrame <- nFrames
  body(po) <- Bo
#  environment(po) <- Envir
#  if(plot.it){
#    po()    
#    tst <- eval(po, Envir)
#  }
  invisible(po)
}