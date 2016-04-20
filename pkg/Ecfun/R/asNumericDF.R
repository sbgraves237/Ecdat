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
                        orderBy=NA){
#
  if(is.function(keep)){
      Keep <- sapply(x, keep)
  } else Keep <- 1:ncol(x)
  x. <- x[, Keep]
#
  X <- lapply(x., asNumericChar)
#
#  if(missing(orderBy)){
#      orderBy <- 1:length(X)
#  }
  if((length(orderBy)<2) && is.na(orderBy)){
    out <- as.data.frame(X)
  } else {
    o <- do.call(order, X[orderBy])
    out <- as.data.frame(X)[o, ]
  }
  out 
}

