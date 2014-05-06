interpChar <- function(x, ...){
  UseMethod('interpChar')
}

interpChar.list <- function(x, .proportion, 
                            argnames=character(3), ...){
  if(length(x)<2){
    lx <- length(x[[1]])
    lp <- length(.proportion)
    if(is.numeric(x[[1]])){
      xNm <- names(x)
      if(is.null(xNm) || (nchar(xNm)<1)){
        warning('numerical interpolation in a list of length 1', 
                '\n returns the input')
      } else {
        warning('numerical interpolation in a list of length 1', 
                '\n with an element named ', xNm, 
                ';  returning the input.')
      }
      return(x[[1]])
    }
    out <- interpChar.default('', x[[1]], .proportion, ...)
    return(out)
  }
  interpChar.default(x[[1]], x[[2]], .proportion, ...)
}

interpChar.default <- function(x, y, .proportion, 
                               argnames=character(3), ...){
##
## 1.  numeric? 
##  
  if(missing(y)){
    if(is.numeric(x)){
      warning('numeric interpolation with one input;', 
              '  returning that.')
      return(x)
    }
    y <- x
    x <- '' 
  } else { 
    num <- (is.numeric(x) && is.numeric(y))
    if(num){
      out <- (x*(1-.proportion) + y*.proportion) 
      return(out)
    }
  }
##
## 2.  not numeric
##
#  2.1.  as.character 
  xc <- as.character(x)
  yc <- as.character(y)
#  2.2.  Same length
  nx <- length(xc)
  ny <- length(yc)
  np <- length(.proportion)
  N <- max(nx, ny, np)
  X <- rep(xc, length=N)
  Y <- rep(yc, length=N)
  P. <- rep(.proportion, length=N)
#  2.3.  number of characters 
  nch.y <- nchar(Y)
  nch.x <- nchar(X)
  swap <- (nch.y<nch.x)
  Z <- Y
  Z[swap] <- X[swap]
  Ny <- nch.y
  Nx <- nch.x
  P <- P. 
  Ny[swap] <- nch.x[swap]
  Nx[swap] <- nch.y[swap]
  P[swap] <- (1-P.[swap])
  dxy <- (Ny-Nx)
  Dxy <- cumsum(dxy)
  DN <- Dxy[N]
  cumCh <- P*DN 
#  
  D.xy <- c(0, Dxy[-N])
  Pd <- (cumCh - D.xy) 
#  Pd[is.na(Pd)] <- 1 
#  Pd. <- pmax(0, pmin(Pd, 1))
  Out <- character(N)
  sely <- round(Nx + Pd)
  Out[sely>0] <- substring(Z[sely>0], 1, sely[sely>0])
##
## 3.  Done 
## 
  Out 
}