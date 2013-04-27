surname <- function(x){
##
## 1.  suffix
##
  suf <- regexpr(', ', x)
  suffix <- substring(x[suf>0], suf[suf>0]+2)
  x2 <- x
  x2[suf>0] <- substring(x[suf>0], 1, suf[suf>0]-1)
##
## 2.  surname
##
  x. <- strsplit(x2, ' ')
  surname <- sapply(x., function(x){
      x[length(x)]
  } )
##
## 3.  givenName
##
  g <- sapply(x., function(x){
      paste(x[1:(length(x)-1)], collapse=' ')
  } )
  g[suf>0] <- paste(g[suf>0], suffix, sep=', ')
##
## 4.  Done
##
  cbind(surname=surname, givenName=g)
}

