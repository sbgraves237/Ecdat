points.qqnorm2 <- function(x, ...){
##
## 1.  arg list:  dots (...) takes precidence over x
##
  o <- order(x$x)
  dots <- list(...)
  dots0 <- dots
  for(i in names(x)){
    if(!(i %in% names(dots0))){
      if(i %in% c('x', 'y')){
        dots0$x <- x$x[o]
        dots0$y <- x$y[o]
      } else if(!(i %in% c('z', 'log'))){
        dots0[[i]] <- x[[i]]
      }
    }
  }
  dots0$log <- NULL 
#   kill any 'log' component 
##
## 2.  if(type %in% c('p', 'b', 'o'))text or points 
##
  if(is.null(dots0$type) || 
        (dots0$type %in% c('p', 'b', 'o'))){
    dotsp <- dots0 
#  2.1.  if(is.null(pch) text(...)
    if(is.null(x$z)){
      z <- rep(1, length(x$x))
    } else z <- x$z[o]
    if(is.null(dots0$pch)){
##**** NOTE:  The behavior here may not match 
##**** the documentation if
## z is integer between 0 and 255      
##**** lines.qqnorm2 should work properly      
##**** out of time to fix this 2018      
      dotsp$labels <- z
      do.call(text, dotsp)
    } else if(is.character(dots0$pch)){
       dotsp$labels <- dots0$pch[z]
       dotsp$type <- NULL
       do.call(text, dotsp)
    } else {
       dotsp$type <- 'p'
       dotsp$pch <- dots0$pch[z]
       do.call(points, dotsp)
    }
  }
##    
## 3.  if(type != 'p')lines(x$x, x$y, ...);  
##     a call to points should ignore type = p or n  
##
  if((!is.null(dots0$type)) &&
           (dots0$type != 'p')){
    dotsl <- dots0 
    if(dots0$type %in% c('b', 'o'))
      dotsl$type <- c(b='c', o='l')[dots0$type]
    do.call(lines, dotsl)
  }
##
## 4.  points.default returns NULL 
##
  NULL
} 