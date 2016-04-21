asNumericChar <- function(x){
##
## 1.  Convert factors to character
## 
  X <- x
  if(is.factor(x))x <- as.character(X)
##
## 1.  Delete leading blanks and $ 
##
  x <- tis::stripBlanks(x)
  dol <- grep('^\\$', x)
  x[dol] <- sub('^\\$', '', x[dol])
##
## 2.  find percent
##  
  pct <- grep('%$', x)
  x0 <- sub('%$', '', x)
##
## 3.  Delete commas (thousand separators) and footnote references
##
  x1 <- gsub(',', '', x0)
  x2 <- strsplit(x1, ' ')
  x. <- sapply(x2, '[', 1)
  x.[x1==''] <- NA
  xo <- as.numeric(x.)
##
## 4.  rescale percents 
##
  xo[pct] <- xo[pct]/100
  xo
}

asNumericDF <- function(x, keep=function(x)any(!is.na(x)),
        orderBy=NA, ignore=NULL, factors=NULL, Dates=NULL, 
        POSIX=NULL, format){
##
## 1.  Copy x
##  
  X <- as.data.frame(x)
##  
## 2.  Confirm that ignore, factors, Dates, and POSIX
##     all refer to columns of x and do not overlap.  
##
  k <- ncol(x)
  Names <- colnames(x)
#   check for Names in referenceList    
  if(is.numeric(ignore)){
    if(any(ignore<1)){
      stop('numeric ignore < 1')
    }
    if(any(ignore>k)){
      stop('ignore numeric > ncol(x)')
    }
    ignore <- colnames(x)[ignore]
  } else {
    if(length(igoops <- which(!(ignore %in% Names)))>0){
      stop('ignore = ', ignore[igoops[1]], 
           ' not in names(x) = ', 
           paste(Names, collapse=', ') )
    }
  }
# skip tests of factors, Dates, and POSIX
# ; implement later
##  
## 3.  Convert factors, Dates, and POSIX 
##
  for(f in factors){
    X[, f] <- factor(x[, f])
  }
  for(d in Dates){
    X[, d] <- as.Date(x[, d], format)
  }
  for(p in POSIX){
    if(missing(format)){
      X[, p] <- as.POSIXct(x[, p])
    } else {
      X[, p] <- as.POSIXct(x[, p], format)
    }
  }
##
## 4.  Apply asNumericChar to all columns 
##     not in ignore, factors, Dates, or POSIX.  
##
  dontConvert <- union(ignore, union(factors, 
                            union(Dates, POSIX)))
  notNum <- (Names %in% dontConvert)
  numCols <- Names[!notNum]
  for(n in numCols){
    w0 <- options(warn=-1)
    xn <- asNumericChar(x[, n])
    options(warn=w0$warn)
    xnNewNA <- which(is.na(xn) & !is.na(x[, n]))
    if(length(xnNewNA)>0){
      msg0 <- paste0('NAs introduced by coercion ', 
         'in asNumericChar(c(' )
      if(length(xnNewNA)>4){
        msg1 <- paste0(msg0, paste(xnNewNA[1:4], collapse=', '), 
                       ', ...')
      } else {
        msg1 <- paste0(msg0, paste(xnNewNA, collapse=', '))
      }
      msg <- paste0(msg1, '), ', n, ')')
      warning(msg)
    }
    X[, n] <- xn
  }
##
## 5.  Keep columns specified by keep.  
##
  kp <- rep(FALSE, k)
  names(kp) <- Names
  kp[notNum] <- TRUE
  if(is.function(keep)){
      Keep <- sapply(X[numCols], keep)
      kp[numCols[Keep]] <- TRUE
  } else {
    if(!is.null(keep)) {
      if(is.logical(keep)) {
        kp[keep] <- TRUE 
      } else {
        kp[keep] <- TRUE 
      }
    }
  }
#
#  if(missing(orderBy)){
#      orderBy <- 1:length(X)
#  }
  if((length(orderBy)>1) && !is.na(orderBy)){
    o <- do.call(order, X[orderBy])
    return(X[o, kp])
  }
  X[, kp]
}

