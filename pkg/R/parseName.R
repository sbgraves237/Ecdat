parseName <- function(x, surnameFirst=FALSE,
          suffix=c('Jr.', 'I', 'II', 'III', 'IV', 'Sr.'),
          fixNonStandard=subNonStandardNames, ...){
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
  Suffix <- substring(x[suf>0], suf[suf>0]+2)
  x2 <- x
  x2[suf>0] <- substring(x[suf>0], 1, suf[suf>0]-1)
#
  suf. <- regexpr(',', x2)
  Suffix. <- substring(x[suf.>0], suf.[suf.>0]+1)
  x2[suf.>0] <- substring(x[suf.>0], 1, suf.[suf.>0]-1)
##
## 3.  surname
##
  x. <- strsplit(x2, ' ')
  surname <- sapply(x., function(x){
      x[length(x)]
  } )
##
## 4.  check suffix misidentified as surname
##
  oops <- (surname %in% suffix)
  if(any(oops)){
      sufo <- surname[oops]
      shorten <- function(y){
          shorti <- seq(length.out=length(y)-1)
          y[shorti]
      }
      x.o <- lapply(x.[oops], shorten )
      sur.o <- sapply(x.o, function(x){
          x[length(x)]
      } )
      surname[oops] <- sur.o
      x.[oops] <- x.o
  }
##
## 5.  givenName
##
  g <- sapply(x., function(x){
      paste(x[1:(length(x)-1)], collapse=' ')
  } )
  g[suf>0] <- paste(g[suf>0], Suffix, sep=', ')
  g[suf.>0] <- paste(g[suf.>0], Suffix., sep=', ')
  if(any(oops)){
      g[oops] <- paste(g[oops], sufo, sep=', ')
  }
##
## 6.  fixNonStandard
##
  Sur <- fixNonStandard(surname, ...)
  Giv <- fixNonStandard(g, ...)
##
## 7.  Done
##
  cbind(surname=Sur, givenName=Giv)
}

