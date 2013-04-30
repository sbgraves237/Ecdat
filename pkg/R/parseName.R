parseName <- function(x, surnameFirst=FALSE){
##
## 1.  surnameFirst
##
  if(length(x)<1){
      warning("length(x) == 0")
      mat0 <- matrix(character(0), nrow=0, ncol=2,
         dimnames=list(NULL, c("surname", "givenName")))
      return(mat0)
  }
#
  if(surnameFirst){
      Sep <- regexpr(', ', x)
      oops <- which(Sep<0)
      if((no <- length(oops))>0){
          err <- paste(no, ' of ', length(x), ' elements of x ',
                       'are NOT in surnameFirst format.  ',
                       'The first is ', x[oops[1]],
               '.  Assume they are in (givenName surname) format.')
          warning(err)
          fix0 <- parseName(x[oops])
          fix1 <- paste(fix0[, 2], fix0[, 1], sep=', ')
          x[oops] <- fix1
      }
      sur <- substring(x, 1, Sep-1)
      giv <- substring(x, Sep+2)
      out <- cbind(sur, giv)
      colnames(out) <- c('surname', 'givenName')
      return(out)
  }
##
## 2.  suffix?
##
  suf <- regexpr(', ', x)
  suffix <- substring(x[suf>0], suf[suf>0]+2)
  x2 <- x
  x2[suf>0] <- substring(x[suf>0], 1, suf[suf>0]-1)
##
## 3.  surname
##
  x. <- strsplit(x2, ' ')
  surname <- sapply(x., function(x){
      x[length(x)]
  } )
##
## 4.  givenName
##
  g <- sapply(x., function(x){
      paste(x[1:(length(x)-1)], collapse=' ')
  } )
  g[suf>0] <- paste(g[suf>0], suffix, sep=', ')
##
## 5.  Done
##
  cbind(surname=surname, givenName=g)
}

