plot.qqnorm2s <- function(x, y, ...) {
##
## 1.  establish the scale
##
    dots <- list(...)
    legend. <- x$legend
    x$legend. <- NULL
    k <- length(x)
    y. <- names(x)
    ylim. <- xlim. <- matrix(NA, 2, k, dimnames=
                           list(c('min', 'max'), y.) )
    xlab. <- ylab. <- character(k)
    Xlab <- Ylab <- TRUE
    xlab <- ylab <- NULL 
    for(i in seq(length=k)){
        xlim.[, i] <- range(x[[i]]$x, na.rm=TRUE)
        ylim.[, i] <- range(x[[i]]$y, na.rm=TRUE)
        if(!is.null(x[[i]]$xlab)){
          xlab.[i] <- x[[i]]$xlab
#   Is xlab constant?  
          if(is.null(xlab)){
            xlab <- x[[i]]$xlab 
          } else {
            if(xlab != x[[i]]$xlab)Xlab <- FALSE 
          }
        }
        if(!is.null(x[[i]]$ylab)){
          ylab.[i] <- x[[i]]$ylab
#   Is ylab constant?          
          if(is.null(ylab)){
            ylab <- x[[i]]$ylab 
          } else {
            if(ylab != x[[i]]$ylab)Ylab <- FALSE 
          }
        }
    }
    dots$x <- range(xlim.)
    dots$y <- range(ylim.)
    if(!('xlab' %in% names(dots))){
        if(Xlab){
            dots$xlab <- xlab
        } else dots$xlab <- ''
    }
    if(!('ylab' %in% names(dots))){
        if(Ylab){
            dots$ylab <- ylab
        } else dots$ylab <- ''
    }
    dots$type <- 'n'
    if(!('log' %in% names(dots)))
        dots$log <- x[[1]]$log
##
## 2.  create the naked plot
##
    do.call(plot, dots)
##
## 3.  plot the lines and points
##
    for(i in 1:k){
        xi <- x[[i]]
        xi$log <- NULL
#        lines(xi, type='b', ...)
        if(!('type' %in% names(xi)))xi$type <- 'b'
#        do.call(lines.qqnorm2, xi)
        lines.qqnorm2(xi)
    }
##
## 4.  legend
##
    for(i in 1:length(legend.)){
      if(!is.null(legend.[[i]]$legend))
          do.call(legend, legend.[[i]])
    }
}
