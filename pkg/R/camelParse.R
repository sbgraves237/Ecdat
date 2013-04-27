camelParse <- function(x){
##
## 1.  strsplit
##
  x. <- strsplit(x, "")
##
## 2.  Find lower followed by upper
##
  nx <- length(x)
  out <- vector('list', length=nx)
  names(out) <- names(x)
  for(ix in 1:nx){
    xi <- x.[[ix]]
    lower <- (xi %in% letters)
    upper <- (xi %in% LETTERS)
    ni <- length(xi)
    camel <- which(lower[-ni] & upper[-1])
    begin <- c(1, camel+1)
    end <- c(camel, ni)
    X <- substring(x[ix], begin, end)
    out[[ix]] <- X
  }
  out
}

