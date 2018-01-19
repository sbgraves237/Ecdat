qqnorm2 <- function(y, z, plot.it=TRUE, datax=TRUE, pch=NULL, ...){
##
## 1.  qqnorm
##
    dots <- list(...)
    if(!('xlab' %in% names(dots))){
        if(datax){
            dots$xlab <- deparse(substitute(y))
        } else {
            dots$xlab <- 'Normal scores'
        }
    }
    if(!('ylab' %in% names(dots))){
        if(datax){
            dots$ylab <- 'Normal scores'
        } else {
            dots$ylab <- deparse(substitute(x))
        }
    }
    q2 <- qqnorm(y, datax=datax, plot.it=FALSE, ...)
    dots$x <- q2$x
    dots$y <- q2$y
##
## 2.  z and pch
##
    if(is.null(pch)){
        if(is.logical(z) || all(z %in% 0:1)){
            if(is.logical(z)){
                pch <- c('FALSE'=4, 'TRUE'= 1)
            } else {
                pch <- c('0'=4, '1'=1)
                z <- as.character(z)
            }
        } else if(is.character(z) || is.factor(z)){
            z <- as.character(z)
            pch <- unique(z)
            names(pch) <- pch
        } else {
            zi <- as.integer(round(z))
            frac <- z-zi
            if(all(abs(frac) < sqrt(.Machine$double.eps))){
                z <- zi
                zmax <- max(z)
                if((min(z)>0) && (zmax<256)){
                    pch <- 1:zmax
                    names(pch) <- pch
                }
            }
        }
    } else {
        if(is.null(names(pch))){
            if(!is.numeric(z)){
                stop('z must be numeric if is.null(names(pch))')
            }
            maxz <- max(z)
            if(length(pch)<trunc(maxz)){
                stop('max(z) =', maxz, '> length(pch) =', length(pch))
            }
            minz <- min(z)
            if(minz<1){
                stop('min(z) =', minz, '< 1')
            }
        } else {
            yes <- (z %in% names(pch))
            if(any(!yes)){
                stop('element ', which(!yes)[1], 
                     ' is not in names(pch)')
            }
        }
    }
    dots$z <- z
    dots$pch <- pch
##
## 4.  done
##
    class(dots) <- 'qqnorm2'
    if(plot.it)plot(dots)
    invisible(dots)
}