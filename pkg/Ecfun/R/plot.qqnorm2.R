plot.qqnorm2 <- function(x, y, ...){
##
## 1.  arg list:  dots (...) takes precidence over x
##
#    n <- length(x$x)
    dots <- list(...)
    if('type' %in% names(dots)){
        type <- dots$type
    } else if('type' %in% names(x)){
        type <- x$type
    } else type <- 'p'
##
## 2.  create blank plot:  plot(x$x, y$y, type='n', ...)
##
    dots0 <- dots
    o <- order(x$x)
    for(i in names(x)){
      if(!(i %in% names(dots0))){
        if(i %in% c('x', 'y')){
          dots0$x <- x$x[o]
          dots0$y <- x$y[o]
        } else if(i != 'z'){
          dots0[[i]] <- x[[i]]
        }
      }
    }
    dots0$type <- 'n'
    do.call(plot, dots0)
##
## 3.  plot line only (without z and pch)
##
    if(type %in% 
         c('l', 'b', 'c', 'o', 'h', 's', 'S')){
      dotsl <- dots0
      if(dots0$type %in% c('b', 'o'))
          dotsl$type <- c(b='c', o='l')[dots0$type]
#         type = line only       
      do.call(lines, dotsl)
    }
##
## 4.  plot points only 
##
    if(type %in% c('p', 'b', 'o')){
      dotsp <- dots0
      dotsp$type <- NULL 
#     if(is.null(x$z))points(x$x, x$y, ...)
      if(is.null(x$z)){
        do.call(points, dotsp)
      } else if(is.logical(x$z)){
#       else if(is.logical(x$z))
#              points(x$x, x$y, pch=x$pch[is.character(x$z)], ...)
        if(is.null(dotsp$pch)){
          dotsp$pch <- c('FALSE'=4, 'TRUE'=1)[as.character(x$z[o])]
        } else dotsp$pch <- dotsp$pch[as.character(x$z[o])]  
        do.call(points, dotsp)
      } else if(is.numeric(x$z) && (min(z0 <- round(x$z))>0)
              && (max(abs(x$z-z0))<10*.Machine$double.eps)){
#         points(x$x, x$y, pch=x$pch[x$z], ...)
        if(is.null(dotsp$pch)){
          dotsp$pch <- z0[o] 
        } else dotsp$pch <- dotsp$pch[z0[o]]
        do.call(points, dotsp)
      } else {
#         text(x$x, x$y, x$z, ...)
          dotsp$labels <- x$z[o]
          do.call(text, dotsp)
      }
    }
##
## 5.  plot.default returns NULL
##
    NULL
}