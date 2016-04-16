Newdata <- function(data, x, n=31, na.rm=TRUE){
##
## 1.  check data, x
##
  if(length(x) != 1)
    stop('length(x) must be 1; is ', length(x))
  if(is.numeric(x))x <- colnames(data)[x]
  vars <- colnames(data)
  if(!(x %in% vars))
    stop('x = ', x, ' not in colnames(data) = ', 
         paste(vars, collapse=', '))
##
## 2.  canbeNumeric(x) 
##
  x. <- data[, x]
  if(canbeNumeric(x.)){
    xn <- as.numeric(x.)
    xrng <- range(xn)
    xNew <- seq(xrng[1], xrng[2], length=n)
    attributes(xNew) <- attributes(x.)
  } else {
    xlvls <- levels(x.)
    if(!is.null(xlvls)){
      n <- length(xlvls)
      xNew <- x.[rep(1, n)]
      for(i in 1:n){
        xNew[i] <- xlvls[i]
      } 
    } else xNew <- NULL
  }
##
## 3.  is.null(xNew) fix
## 
  if(is.null(xNew)) {
    xNew <- sort(unique(x.))
    n <- length(xNew)
  }
##
## 4.  create newDat and set x
##
  newDat <- data[rep(1, n), , drop=FALSE]
  rownames(newDat) <- NULL
  newDat[, x] <- xNew 
##
## 5.  otherVars <- vars[!(vars == x)]
##
  otherVars <- vars[!(vars==x)]
##
## 6.  Replace otherVars as desired
##
  for(x2 in otherVars){
    x2. <- data[, x2]
    if(canbeNumeric(x2.)){
      x2m <- median(as.numeric(x2.))
      attributes(x2m) <- attributes(x2.)
      newDat[, x2] <- x2m
    } else {
      x2t <- table(x2.)
      x2s <- sort(x2t, decreasing=TRUE)
      x2new <- names(x2s[1])
      sel <- which(data[, x2] == x2new)
      newDat[, x2] <- data[sel[1], x2]
    }
  }
##
## 8.  done 
##  
  newDat
}
