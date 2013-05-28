parseName <- function(x, surnameFirst=(median(regexpr(',', x))>0),
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
  if(missing(surnameFirst))
      surnameFirst <- surnameFirst
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
      Sur <- fixNonStandard(sur, ...)
      Giv <- fixNonStandard(giv, ...)
      out <- cbind(Sur, Giv)
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
# check for a comma without a space
  suf. <- regexpr(',', x2[suf<=0])
  if(any(suf.>0)){
      Suffix. <- substring(x2[suf<=0][suf.>0], suf.[suf.>0]+1)
      x2[suf<=0][suf.>0] <- substring(x2[suf<=0][suf.>0],
                                     1, suf.[suf.>0]-1)
  }
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
  sufSur <- (surname %in% suffix)
  if(any(sufSur)){
      sufo <- surname[sufSur]
      shorten <- function(y){
          shorti <- seq(length.out=length(y)-1)
          y[shorti]
      }
      x.o <- lapply(x.[sufSur], shorten )
      sur.o <- sapply(x.o, function(x){
          x[length(x)]
      } )
      surname[sufSur] <- sur.o
      x.[sufSur] <- x.o
  }
##
## 5.  givenName
##
  g <- sapply(x., function(x){
      paste(x[1:(length(x)-1)], collapse=' ')
  } )
  g[suf>0] <- paste(g[suf>0], Suffix, sep=', ')
#  comma without a space
  if(any(suf.>0)){
      g[suf<=0][suf.>0] <- paste(g[suf<=0][suf.>0], Suffix., sep=', ')
  }
#  suffix misidentified as surname
  if(any(sufSur)){
      g[sufSur] <- paste(g[sufSur], sufo, sep=', ')
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

