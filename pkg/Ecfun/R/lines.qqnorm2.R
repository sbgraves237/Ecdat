lines.qqnorm2 <- function(x, ...){
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
## 2.  if(type != 'p')lines(x$x, x$y, ...);  
##     a call to lines should ignore type = p or n  
##
  dotsl <- dots0
  if(is.null(dots0$type)){
    do.call(lines, dotsl)
  } else {
    if(dots0$type != 'p'){
      if(dots0$type %in% c('b', 'o'))
        dotsl$type <- c(b='c', o='l')[dots0$type]
      do.call(lines, dotsl)
    }
#  !is.null(dots0$type)
##
## 3.  if(type %in% c('p', 'b', 'o'))text or points 
##
    if(dots0$type %in% c('p', 'b', 'o')){
      dotsp <- dots0 
      if(is.null(x$z)){
        z <- rep(1, length(x$x))
      } else z <- x$z[o]
#  3.1.  if(is.null(pch) text(...)
      if(is.null(dots0$pch)){
        if(is.numeric(z)){
          zi <- as.integer(round(z))
          frac <- z-zi
          if(all(abs(frac) < sqrt(.Machine$double.eps))){
            z <- zi
            zmax <- max(z)
            if((min(z)>=0) && (zmax<256)){
               pch <- 1:zmax
               names(pch) <- pch
               dotsp$type <- 'p'
               dotsp$pch <- pch[z]
               do.call(points, dotsp)
            } else {
               dotsp$labels <- z
               do.call(text, dotsp)
            } 
          } else {
               dotsp$labels <- z
               do.call(text, dotsp)
          } 
        } else {
          dotsp$labels <- z
          do.call(text, dotsp)
        }
#   3.2. !is.null(dots0$pch)
      } else if(is.character(dots0$pch)){
        if(is.logical(z))z <- as.character(z)
        dotsp$labels <- dots0$pch[z]
        dotsp$type <- NULL
        do.call(text, dotsp)
      } else {
        dotsp$type <- 'p'
        if(is.logical(z))z <- as.character(z)
        dotsp$pch <- dots0$pch[z]
        do.call(points, dotsp)
      }
    }
  }
##
## 4.  lines.default returns NULL 
##
  NULL
}