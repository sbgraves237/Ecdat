qqnorm2s <- function(y, z=NULL, data., plot.it=TRUE, datax=TRUE, 
        outnames=NULL, pch=NULL, col=c(1:4, 6), legend.=NULL, ...){
##
## 1.  Check lengths of y, z, data.
##
    ky <- length(y)
    kz <- length(z)
    if(is.data.frame(data.)){
        kd <- 1
        data. <- list(data.)
    } else kd <- length(data.)
    k. <- max(ky, kz, kd)
    ko <- length(outnames)
    if(ko != k.){
      if(kd==k.){
          outnames <- names(data.)  
          ko <- k.
      } 
      if(is.null(outnames) && (ky==k.)){
          outnames <- y
          ko <- k.
      }
#
      if(is.null(outnames)){
        outnames <- 1:k. 
        ko <- k.
      }
      if(ko<k.) 
        warning('length(outnames) = ', ko, 
                ' != max length of y, z, data.\n',
                ' ignoring outnames.')
    }
##
## 2.  Match lengths
##
    y <- rep(y, length=k.)
    if(kz>0)z <- rep(z, length=k.)
#    if(is.data.frame(data.)) data. <- list(data.) # moved up 
    if(kd<k.){
        for(i in (kd+1):k.){
            data.[[i]] <- data.[[1]]
        }
    }
##
## 2.  create a list of objects of class qqnorm2s
##
    dots <- list(...)
    Dots <- dots
    if(!('type' %in% names(Dots))) Dots$type <- 'b'
#    Dots$plot.it <- plot.it
    Dots$plot.it <- FALSE 
    Dots$datax <- datax
    Dots$pch <- pch
#    Dots$z <- data.[, z]
    col <- rep(col, length=k.)
    pch. <- qq2s <- vector('list', k.)
    if(length(outnames) == k.) {
        names(qq2s) <- outnames
    } else {
      warning('length(outnames) = ', length(outnames), 
              ' != number of lines;  no legend for lines')
    }
    for(i in seq(length=k.)){
#       qq2s[[i]] <- qqnorm2(data.[, y[i]], data.[, z], plot.it=FALSE,
#                            datax=datax, pch=pch, ...)
#       allow for partial matching
        namesi <- names(data.[[i]])
        if(y[i] %in% namesi){
            yi <- y[i]
        } else yi <- grep(y[i], namesi)
        oi <- order(data.[[i]][, yi])
        Dots$y <- data.[[i]][oi, yi]
        if(kz>0){
          if(z[i] %in% namesi){
              zi <- z[i]
          } else zi <- grep(z[i], namesi)
          Dots$z <- data.[[i]][oi, zi]
          qq2s[[i]] <- do.call(qqnorm2, Dots)
        } else {
          qq2s[[i]] <- do.call(qqnorm, Dots)
        }
#
        qq2s[[i]]$col <- col[i]
        if(datax){
            qq2s[[i]]$xlab <- y[i]
            qq2s[[i]]$ylab <- 'Normal scores'
        } else {
            qq2s[[i]]$ylab <- y[i]
            qq2s[[i]]$xlab <- 'Normal scores'
        }
        if(!is.null(qq2s[[i]]$pch)) 
              pch.[[i]] <- qq2s[[i]]$pch
    }
    class(qq2s) <- 'qqnorm2s'
##
## 3.  add legend.
##
    if(is.null(legend.)){
        nch <- sapply(pch., length)
        nchi <- which.max(nch)
        Pchi <- pch.[[nchi]]
        if(!is.null(Pchi)){
          pchOK <- TRUE
          for(i in seq(length=k.)){
              chki <- all.equal(Pchi, pch.[[i]])
              if(chki != TRUE){
                  cat('pch[', nchi, '] != pch[', i, ']\n')
                  print(pch[[i]])
                  pchOK <- FALSE
                  warning('pch not the same for all lines')
              }
          }
          if(!pchOK){
            cat('Legend will display pch =\n')
            print(Pchi)
          }
          namesPch <- names(Pchi)
          if(is.null(namesPch))
              namesPch <- 1:length(Pchi)
          legend.$pch <- list(x='right', legend=namesPch, pch=Pchi)
        }
#  If only one line, is.null(names(qq2s))
        legend.$col <- list(x='bottomright', legend=names(qq2s),
                            lty=1, col=col)
    }
    qq2s$legend. <- legend.
##
## 4.  done
##
    if(plot.it)plot(qq2s)
    invisible(qq2s)
}