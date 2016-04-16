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
## 2.  x.rng
##
  x.rng <- range(data[, x], na.rm=na.rm)
##  
## 3.  newDat 
##
  newDat <- data[rep(1, n), , drop=FALSE]
  rownames(newDat) <- NULL
##  
## 4.  newDat[, x] 
##
  newDat[, x] <- seq(x.rng[1], x.rng[2], length=n)
##
## 5, 6.  otherVars 
##
  otherVars <- vars[!(vars == x)]
##
## 7.  Replace otherVars as desired
##
  for(x2 in otherVars){
    if(is.character(data[, x2])){
      x2t <- table(data[, x2])
      newDat[, x2] <- names(sort(x2t, decreasing=TRUE)[1])
      next
    }
    suppressWarnings(x2n <- as.numeric(data[, x2]))
    Lvl <- ('levels' %in% names(attributes(data[, x2])))
    notNum <- any(is.na(x2n) != is.na(data[, x2]))
    if(Lvl | notNum){
      x2t <- table(data[, x2])
      sel <- which(data[, x2] == names(sort(x2t, descreasing=TRUE)[1]))
      newDat[, x2] <- data[sel[1], x2]
      next
    }
    x2m <- median(x2n)
    attributes(x2m) <- attributes(data[, x2])
    newDat[, x2] <- x2m
  }
##
## 8.  done 
##  
  newDat
}
