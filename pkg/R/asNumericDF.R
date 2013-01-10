asNumericChar <- function(x){
# Delete commas (thousand separators) and footnote references
    x1 <- gsub(',', '', x)
    x2 <- strsplit(x1, ' ')
    x. <- sapply(x2, '[', 1)
    x.[x1==''] <- NA
    as.numeric(x.)
}

asNumericDF <- function(x, keep=function(x)any(!is.na(x)),
                        orderBy){
#
  if(is.function(keep)){
      Keep <- sapply(x, keep)
  } else Keep <- 1:ncol(x)
  x. <- x[, Keep]
#
  X <- lapply(x., asNumericChar)
#
  if(missing(orderBy)){
      orderBy <- 1:length(X)
  }
  o <- do.call(order, X[orderBy])
  as.data.frame(X)[o, ]
}

