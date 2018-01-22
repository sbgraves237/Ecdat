qqnorm2t <- function(y, x, z=NULL, data., plot.it=TRUE, datax=TRUE, 
        outnames=NULL, pch=NULL, col=c(1:4, 6), legend.=NULL, ...){
#
  dat2 <- split(data., data.[x])
  if(is.null(outnames))outnames <- names(dat2)
#
  qqnorm2s(y, z, dat2, plot.it, datax, outnames, pch, col, legend., ...)
}
