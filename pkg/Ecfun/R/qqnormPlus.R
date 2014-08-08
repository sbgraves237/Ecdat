qqnormPlus <- function(y, xlim, ylim, main = "",
          xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
          datax = TRUE, histargs, densityargs, boxplotargs, ...){
##
## 0.  require(grid, gridbase)  
##
  require(grid)
  require(gridBase)
##
## 1.  qqnorm
##
  qq <- qqnorm(y, plot.it=FALSE, datax=datax)
##
## 2.  plot(qq, ...)
##
  if(datax){
    if(missing(xlim)){
#      xlim <- 
    }
    
    
  }
  
  if(missing(xlim)){
    if(datax){
      xlim <- range(qq$y)
    } else {
      xlim <- range(qq$x)
    }
  }
  if(missing(ylim)){
    if(datax){
      ylim <- range(qq$x)
    } else {
      ylim <- range(qq$y)
    }
  }
  

#
  plot(qq, xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, 
       bty='n', xpd='na', ...) 
##
## 3.  gridBase
##
  vps <- baseViewports()
#  pushViewport(vps$inner, vps$figure, vps$plot)
  pushViewport(vps$inner)
  on.exit(popViewport(), add=TRUE) 
  pushViewport(viewport(clip='off'))
  on.exit(popViewport(), add=TRUE) 
##
## 4.  hist 
##

    
    
    
    
    
  
  
}

